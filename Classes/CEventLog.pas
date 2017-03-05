unit CEventLog;

interface

uses CTemplateEntity;

type TypeEventLogs = (elNone,
                      elProgramStart, //включение программы
                      elProgramStop,  //выключение программы
                      elWorkModeOn,   //включение рабочего режима
                      elWorkModeOff,  //выключение рабочего режима
                      elTempRangeOut, //выход за границы температурного режима
                      elTempRangeIn,  //возврат температурного режима в нормальный диапазон
                      elSignalOn,     //включение сигнализации
                      elSignalOff,    //отключение сигнализации
                      elChangeRanges, //изменение границ температурного диапазона
                      elChangePass    //изменение пароля администратора
                      );

type
  TMEventLog = class(TMTemplateEntity)
    constructor Create();
  private
    FEventLogId : variant;

    FEventLogType    : TypeEventLogs;
    FEventLogDetails : string;
    FEventLogTime    : TDateTime;

  protected
    procedure GetTableRecordData(); override;
    procedure SetTableRecordData(); override;
    procedure SetPKFieldValue(Value : variant); override;

    procedure Reset(); virtual;

  public
    function WriteLog(AEventLogType : TypeEventLogs; ADetails : string) : boolean; overload;
    function WriteLog(AEventLogType : TypeEventLogs) : boolean; overload;

  public
    property EventLogId      : variant       read FEventLogId;
    property EventLogType    : TypeEventLogs read FEventLogType    write FEventLogType;
    property EventLogDetails : string        read FEventLogDetails write FEventLogDetails;
    property EventLogTime    : TDateTime     read FEventLogTime    write FEventLogTime;
  end;


function EventLogToStr(ALogEvent: TypeEventLogs): string;
function StrToEventLog(AStr: string): TypeEventLogs;

implementation

uses SysUtils;

function EventLogToStr(ALogEvent: TypeEventLogs): string;
const
  LogEventStrings: array[TypeEventLogs] of string =
 ('None', 'ProgramStart', 'ProgramStop', 'WorkModeOn', 'WorkModeOff',
  'TempRangeOut', 'TempRangeIn', 'SignalOn', 'SignalOff', 'ChangeRanges',
  'ChangePass');
begin
  Result := LogEventStrings[ALogEvent];
end;

function StrToEventLog(AStr: string): TypeEventLogs;
var
  I: TypeEventLogs;
begin
  I := Low(TypeEventLogs);
  while (I <= High(TypeEventLogs)) do
    begin
      if UpperCase(AStr) = UpperCase(EventLogToStr(TypeEventLogs(I)))
        then Break;
      I := Succ(I);
    end;

  Result := I;
end;


//////////////////////TMEventLog////////////////////////////////////////////////
constructor TMEventLog.Create;
begin
  FTableName := 'EventLogs';

  inherited;

  Reset;
end;


procedure TMEventLog.GetTableRecordData();
var
  tmpStr : string;
begin
  with FTableRecord do
  begin
    FEventLogId := FieldByName['EventLogId'].AsInteger;

    tmpStr := FieldByName['CheckBoxField'].AsString;
    FEventLogType := StrToEventLog(tmpStr);

    FEventLogDetails := FieldByName['EventLogDetails'].AsString;
    FEventLogTime    := FieldByName['EventLogTime'].AsDateTime;
  end;
end;

procedure TMEventLog.SetTableRecordData();
var
  tmpStr : string;
begin
  with FTableRecord do
  begin
    FieldByName['EventLogId'].Value := FEventLogId;

    tmpStr := EventLogToStr(FEventLogType);
    FieldByName['EventLogType'].Value := tmpStr;

    FieldByName['EventLogDetails'].Value := FEventLogDetails;
    FieldByName['EventLogTime'].Value    := FEventLogTime;
  end;
end;


procedure TMEventLog.SetPKFieldValue(Value : variant);
begin
  FEventLogId := Value;
end;

function TMEventLog.WriteLog(AEventLogType : TypeEventLogs; ADetails : string) : boolean;
begin
  FTableRecord.ClearRecordValues;

  FTableRecord.FieldByName['EventLogType'].Value    := EventLogToStr(AEventLogType);
  FTableRecord.FieldByName['EventLogDetails'].Value := ADetails;
  FTableRecord.FieldByName['EventLogTime'].Value    := Now;

  Result := (FTableRecord.AddRecord > 0);
end;

function TMEventLog.WriteLog(AEventLogType : TypeEventLogs) : boolean;
begin
  Result := WriteLog(AEventLogType, '');
end;

procedure TMEventLog.Reset();
begin
  inherited;

  FEventLogId := 0;

  FEventLogType    := elNone;
  FEventLogDetails := '';
  FEventLogTime    := 0;
end;

end.
