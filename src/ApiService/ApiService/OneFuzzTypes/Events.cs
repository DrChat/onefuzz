﻿using System.Text.Json;
using System.Text.Json.Serialization;
using Microsoft.OneFuzz.Service.OneFuzzLib.Orm;
using PoolName = System.String;
using Region = System.String;

namespace Microsoft.OneFuzz.Service;




public enum EventType {
    JobCreated,
    JobStopped,
    NodeCreated,
    NodeDeleted,
    NodeStateUpdated,
    Ping,
    PoolCreated,
    PoolDeleted,
    ProxyCreated,
    ProxyDeleted,
    ProxyFailed,
    ProxyStateUpdated,
    ScalesetCreated,
    ScalesetDeleted,
    ScalesetFailed,
    ScalesetStateUpdated,
    ScalesetResizeScheduled,
    TaskCreated,
    TaskFailed,
    TaskStateUpdated,
    TaskStopped,
    CrashReported,
    RegressionReported,
    FileAdded,
    TaskHeartbeat,
    NodeHeartbeat,
    InstanceConfigUpdated,
}

public abstract record BaseEvent() {
    public EventType GetEventType() {
        return
            this switch {
                EventNodeHeartbeat _ => EventType.NodeHeartbeat,
                EventTaskHeartbeat _ => EventType.TaskHeartbeat,
                EventPing _ => EventType.Ping,
                EventInstanceConfigUpdated _ => EventType.InstanceConfigUpdated,
                EventProxyCreated _ => EventType.ProxyCreated,
                EventProxyDeleted _ => EventType.ProxyDeleted,
                EventProxyFailed _ => EventType.ProxyFailed,
                EventProxyStateUpdated _ => EventType.ProxyStateUpdated,
                EventCrashReported _ => EventType.CrashReported,
                EventRegressionReported _ => EventType.RegressionReported,
                EventFileAdded _ => EventType.FileAdded,
                EventTaskFailed _ => EventType.TaskFailed,
                EventTaskStopped _ => EventType.TaskStopped,
                EventTaskStateUpdated _ => EventType.TaskStateUpdated,
                EventScalesetFailed _ => EventType.ScalesetFailed,
                EventScalesetResizeScheduled _ => EventType.ScalesetResizeScheduled,
                EventScalesetStateUpdated _ => EventType.ScalesetStateUpdated,
                EventNodeStateUpdated _ => EventType.NodeStateUpdated,
                EventNodeDeleted _ => EventType.NodeDeleted,
                EventNodeCreated _ => EventType.NodeCreated,
                EventJobStopped _ => EventType.JobStopped,
                _ => throw new NotImplementedException(),
            };

    }

    public static Type GetTypeInfo(EventType eventType) {
        return (eventType) switch {
            EventType.NodeHeartbeat => typeof(EventNodeHeartbeat),
            EventType.InstanceConfigUpdated => typeof(EventInstanceConfigUpdated),
            EventType.TaskHeartbeat => typeof(EventTaskHeartbeat),
            EventType.Ping => typeof(EventPing),
            EventType.ProxyCreated => typeof(EventProxyCreated),
            EventType.ProxyDeleted => typeof(EventProxyDeleted),
            EventType.ProxyFailed => typeof(EventProxyFailed),
            EventType.ProxyStateUpdated => typeof(EventProxyStateUpdated),
            EventType.CrashReported => typeof(EventCrashReported),
            EventType.RegressionReported => typeof(EventRegressionReported),
            EventType.FileAdded => typeof(EventFileAdded),
            EventType.TaskFailed => typeof(EventTaskFailed),
            EventType.TaskStopped => typeof(EventTaskStopped),
            EventType.TaskStateUpdated => typeof(EventTaskStateUpdated),
            EventType.NodeStateUpdated => typeof(EventNodeStateUpdated),
            EventType.ScalesetFailed => typeof(EventScalesetFailed),
            EventType.ScalesetResizeScheduled => typeof(EventScalesetResizeScheduled),
            EventType.ScalesetStateUpdated => typeof(EventScalesetStateUpdated),
            EventType.NodeDeleted => typeof(EventNodeDeleted),
            EventType.NodeCreated => typeof(EventNodeCreated),
            EventType.JobStopped => typeof(EventJobStopped),
            _ => throw new ArgumentException($"invalid input {eventType}"),

        };
    }
};

public class EventTypeProvider : ITypeProvider {
    public Type GetTypeInfo(object input) {
        return BaseEvent.GetTypeInfo((input as EventType?) ?? throw new ArgumentException($"input is expected to be an EventType {input}"));
    }
}

public record EventTaskStopped(
    Guid JobId,
    Guid TaskId,
    UserInfo? UserInfo,
    TaskConfig Config
) : BaseEvent();


public record EventTaskFailed(
    Guid JobId,
    Guid TaskId,
    Error Error,
    UserInfo? UserInfo,
    TaskConfig Config
    ) : BaseEvent();


//record EventJobCreated(
//    Guid JobId,
//    JobConfig Config,
//    UserInfo? UserInfo
//    ) : BaseEvent();


public record JobTaskStopped(
    Guid TaskId,
    TaskType TaskType,
    Error? Error
    ) : BaseEvent();


public record EventJobStopped(
    Guid JobId,
    JobConfig Config,
    UserInfo? UserInfo,
    List<JobTaskStopped> TaskInfo
) : BaseEvent();


//record EventTaskCreated(
//    Guid JobId,
//    Guid TaskId,
//    TaskConfig Config,
//    UserInfo? UserInfo
//    ) : BaseEvent();


public record EventTaskStateUpdated(
    Guid JobId,
    Guid TaskId,
    TaskState State,
    DateTimeOffset? EndTime,
    TaskConfig Config
    ) : BaseEvent();


public record EventTaskHeartbeat(
   Guid JobId,
   Guid TaskId,
   TaskConfig Config
) : BaseEvent();

public record EventPing(
    Guid PingId
) : BaseEvent();

//record EventScalesetCreated(
//    Guid ScalesetId,
//    PoolName PoolName,
//    string VmSku,
//    string Image,
//    Region Region,
//    int Size) : BaseEvent();


public record EventScalesetFailed(
    Guid ScalesetId,
    PoolName PoolName,
    Error Error
) : BaseEvent();


//record EventScalesetDeleted(
//    Guid ScalesetId,
//    PoolName PoolName,

//    ) : BaseEvent();


public record EventScalesetResizeScheduled(
    Guid ScalesetId,
    PoolName PoolName,
    int size
    ) : BaseEvent();


//record EventPoolDeleted(
//    PoolName PoolName
//    ) : BaseEvent();


//record EventPoolCreated(
//    PoolName PoolName,
//    Os Os,
//    Architecture Arch,
//    bool Managed,
//    AutoScaleConfig? Autoscale
//    ) : BaseEvent();


public record EventProxyCreated(
   Region Region,
   Guid? ProxyId
   ) : BaseEvent();


public record EventProxyDeleted(
   Region Region,
   Guid? ProxyId
) : BaseEvent();


public record EventProxyFailed(
   Region Region,
   Guid? ProxyId,
   Error Error
) : BaseEvent();


public record EventProxyStateUpdated(
   Region Region,
   Guid ProxyId,
   VmState State
   ) : BaseEvent();


public record EventNodeCreated(
    Guid MachineId,
    Guid? ScalesetId,
    PoolName PoolName
    ) : BaseEvent();



public record EventNodeHeartbeat(
    Guid MachineId,
    Guid? ScalesetId,
    PoolName PoolName
    ) : BaseEvent();


public record EventNodeDeleted(
    Guid MachineId,
    Guid? ScalesetId,
    PoolName PoolName
) : BaseEvent();


public record EventScalesetStateUpdated(
    Guid ScalesetId,
    PoolName PoolName,
    ScalesetState State
) : BaseEvent();

record EventNodeStateUpdated(
    Guid MachineId,
    Guid? ScalesetId,
    PoolName PoolName,
    NodeState state
    ) : BaseEvent();

public record EventCrashReported(
    Report Report,
    Container Container,
    [property: JsonPropertyName("filename")] String FileName,
    TaskConfig? TaskConfig
) : BaseEvent();

public record EventRegressionReported(
    RegressionReport RegressionReport,
    Container Container,
    [property: JsonPropertyName("filename")] String FileName,
    TaskConfig? TaskConfig
) : BaseEvent();


public record EventFileAdded(
    Container Container,
    [property: JsonPropertyName("filename")] String FileName
) : BaseEvent();


public record EventInstanceConfigUpdated(
    InstanceConfig Config
) : BaseEvent();

public record EventMessage(
    Guid EventId,
    EventType EventType,
    [property: TypeDiscrimnatorAttribute("EventType", typeof(EventTypeProvider))]
    [property: JsonConverter(typeof(BaseEventConverter))]
    BaseEvent Event,
    Guid InstanceId,
    String InstanceName
);

public class BaseEventConverter : JsonConverter<BaseEvent> {
    public override BaseEvent? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
        return null;
    }

    public override void Write(Utf8JsonWriter writer, BaseEvent value, JsonSerializerOptions options) {
        var eventType = value.GetType();
        JsonSerializer.Serialize(writer, value, eventType, options);
    }
}
