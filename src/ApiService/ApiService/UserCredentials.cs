﻿using System.Net.Http.Headers;
using System.Threading.Tasks;
using Microsoft.Azure.Functions.Worker.Http;
using Microsoft.IdentityModel.Tokens;


namespace Microsoft.OneFuzz.Service;

public interface IUserCredentials {
    public string? GetBearerToken(HttpRequestData req);
    public string? GetAuthToken(HttpRequestData req);
    public Task<OneFuzzResult<UserInfo>> ParseJwtToken(HttpRequestData req);
}

public class UserCredentials : IUserCredentials {
    ILogTracer _log;
    IConfigOperations _instanceConfig;

    public UserCredentials(ILogTracer log, IConfigOperations instanceConfig) {
        _log = log;
        _instanceConfig = instanceConfig;
    }

    public string? GetBearerToken(HttpRequestData req) {
        var authHeader = req.Headers.GetValues("Authorization");
        if (authHeader.IsNullOrEmpty()) {
            return null;
        } else {
            var auth = AuthenticationHeaderValue.Parse(authHeader.First());
            return auth.Scheme.ToLower() switch {
                "bearer" => auth.Parameter,
                _ => null,
            };
        }
    }

    public string? GetAuthToken(HttpRequestData req) {
        var token = GetBearerToken(req);
        if (token is not null) {
            return token;
        } else {
            var tokenHeader = req.Headers.GetValues("x-ms-token-aad-id-token");
            if (tokenHeader.IsNullOrEmpty()) {
                return null;
            } else {
                return tokenHeader.First();
            }
        }
    }


    async Task<OneFuzzResult<string[]>> GetAllowedTenants() {
        var r = await _instanceConfig.Fetch();
        var allowedAddTenantsQuery =
            from t in r.AllowedAadTenants
            select $"https://sts.windows.net/{t}/";

        return OneFuzzResult<string[]>.Ok(allowedAddTenantsQuery.ToArray());
    }

    public async Task<OneFuzzResult<UserInfo>> ParseJwtToken(HttpRequestData req) {
        var authToken = GetAuthToken(req);
        if (authToken is null) {
            return OneFuzzResult<UserInfo>.Error(ErrorCode.INVALID_REQUEST, new[] { "unable to find authorization token" });
        } else {
            var token = new System.IdentityModel.Tokens.Jwt.JwtSecurityToken(authToken);
            var allowedTenants = await GetAllowedTenants();
            if (allowedTenants.IsOk) {
                if (allowedTenants.OkV is not null && allowedTenants.OkV.Contains(token.Issuer)) {
                    Guid? applicationId = (
                            from t in token.Claims
                            where t.Type == "appId"
                            select (Guid.Parse(t.Value))).FirstOrDefault();

                    Guid? objectId = (
                            from t in token.Claims
                            where t.Type == "oid"
                            select (Guid.Parse(t.Value))).FirstOrDefault();

                    string? upn = (
                            from t in token.Claims
                            where t.Type == "upn"
                            select t.Value).FirstOrDefault();

                    return OneFuzzResult<UserInfo>.Ok(new(applicationId, objectId, upn));
                } else {
                    _log.Error($"issuer not from allowed tenant: {token.Issuer} - {allowedTenants}");
                    return OneFuzzResult<UserInfo>.Error(ErrorCode.INVALID_REQUEST, new[] { "unauthorized AAD issuer" });
                }
            } else {
                _log.Error("Failed to get allowed tenants");
                return OneFuzzResult<UserInfo>.Error(allowedTenants.ErrorV);
            }
        }
    }
}
