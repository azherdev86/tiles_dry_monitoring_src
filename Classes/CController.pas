unit CController;

interface

uses CIncomingComPortMessage, COutgoingComPortMessage, Classes, CRows;

const CONVEYORS_COUNT = 5;

type
  TypeConveyorWorkMode = (cwmNone,
                          cwmOverlocking,
                          cwmWork);

  TypeSignalMode = (smNone,
                    smEnabled,
                    smDisabled);

type
  TMConveyor = class
    constructor Create;
    destructor Destroy; override;
  private
    FNumber     : integer;
    FWorkMode   : TypeConveyorWorkMode;

    FOutOfRangeSections : TMRow;

    procedure Reset;
    function IsFailure : boolean;
    function GetWorkModeString : string;

  public
    procedure HighLight(ASectionNumber : integer);

    procedure DeHighLight(); overload;
    procedure DeHighLight(ASectionNumber : integer); overload;

    procedure FailureSection(ASectionNumber : integer);
    procedure DeFailureSection(ASectionNumber : integer);

  public
    property Number     : integer                read FNumber;
    property WorkMode   : TypeConveyorWorkMode   read FWorkMode   write FWorkMode;

    property WorkModeString     : string read GetWorkModeString;
    property OutOfRangeSections : TMRow  read FOutOfRangeSections write FOutOfRangeSections;
  end;


type
  TMController = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMConveyor;  overload;
    function GetItem(ItemTitle : string) : TMConveyor; overload;

    function AddItem(Item : TMConveyor) : TMConveyor;

    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

    ////////////////////////////////////////////////////////////
    function CheckTemperatureRanges : boolean;

    function SaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)   : boolean;
    function LoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;  //может этот метод не нужен

    procedure Init;

  private
    Items : TStringList;

    FSignalMode : TypeSignalMode;

    procedure Reset;
    procedure DeHighLight();

    procedure SetSignalMode(ASignalMode : TypeSignalMode);

  public
    property SignalMode : TypeSignalMode read FSignalMode write SetSignalMode;

  end;

implementation

uses SysUtils, LApplicationGlobals, CTempValuesBuffer, Types,
     FMain, CBasicComPortMessage, CEventLog;

///////////////////////TMConveyor//////////////////////////////////

constructor TMConveyor.Create;
begin
  FOutOfRangeSections := TMRow.Create;
end;

destructor TMConveyor.Destroy;
begin
  Reset;

  FOutOfRangeSections.Free;

  inherited;
end;

procedure TMConveyor.Reset;
begin
  FNumber     := 0;

  FWorkMode   := cwmNone;

  FOutOfRangeSections.Clear;
end;

function TMConveyor.IsFailure : boolean;
begin
  Result := FOutOfRangeSections.GetCount > 0;
end;

procedure TMConveyor.HighLight(ASectionNumber : integer);
var
  Point : TPoint;
begin
  Point.X := ASectionNumber - 1;
  Point.Y := CONVEYORS_COUNT - FNumber;

  ApplicationGraph.HighLightCell(Point);
end;

procedure TMConveyor.DeHighLight();
var
  row_index : integer;
begin
  row_index := CONVEYORS_COUNT - FNumber;
  ApplicationGraph.DeHighLightRow(row_index);
end;

procedure TMConveyor.DeHighLight(ASectionNumber : integer);
var
  Point : TPoint;
begin
  Point.X := ASectionNumber - 1;
  Point.Y := CONVEYORS_COUNT - FNumber;

  ApplicationGraph.DeHighLightCell(Point);
end;

procedure TMConveyor.FailureSection(ASectionNumber : integer);
var
  Column : TMColumn;
begin
  Column := FOutOfRangeSections.GetItem(IntToStr(ASectionNumber));

  if Assigned(Column)
    then Exit;

  Column := TMColumn.Create;
  Column.ColumnIndex := ASectionNumber;

  FOutOfRangeSections.AddItem(Column);
end;

procedure TMConveyor.DeFailureSection(ASectionNumber : integer);
var
  Column : TMColumn;
begin
  Column := FOutOfRangeSections.GetItem(IntToStr(ASectionNumber));

  if not Assigned(Column)
    then Exit;

  FOutOfRangeSections.DeleteItem(IntToStr(ASectionNumber));
end;

function TMConveyor.GetWorkModeString : string;
var
  Value : string;
begin
  Value := '';

  case FWorkMode of
    cwmNone:        Value := '';
    cwmOverlocking: Value := 'overlocking';
    cwmWork:        Value := 'work';
  end;

  Result := Value;
end;


procedure TMController.SetSignalMode(ASignalMode : TypeSignalMode);
var
  ComPortMessage : TMOutgoingComportMessage;
begin
  ComPortMessage := TMOutgoingComportMessage.Create;

  FSignalMode := ASignalMode;

  if not SaveToComPortMessage(ComPortMessage)
    then Exit;

  ApplicationComPortOutgoingMessages.AddItem(ComPortMessage);
end;

function TMController.SaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)   : boolean;
var
  DataBytes : TDynamicByteArray;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  SetLength(DataBytes, 2);

  DataBytes[0] := $00;

  case SignalMode of
    smEnabled:  DataBytes[1] := $01;  //включение сигнализации
    smDisabled: DataBytes[1] := $00;  //отключение сигнализации
  end;

  ComPortMessage.LoadDataBytes(DataBytes);
  ComPortMessage.DeviceId  := $02;//FBoxNumber;

  //  ComPortMessage.DebugDeviceId := FBoxNumber;
  ComPortMessage.CommandId := $06;
  ComPortMessage.MSBRegisterAddr := $00;
  ComPortMessage.LSBRegisterAddr := $00;


  ComPortMessage.Priority  := mpHigh;

  ComPortMessage.CreationTime := Now;

  Result := True;
end;


function TMController.LoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;
var
  DataBytes : TDynamicByteArray;
  MessageTime : TDateTime;

  len : integer;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  if ComPortMessage.CommandId <> $06
    then Exit;

  MessageTime := ComPortMessage.RecievedTime;

  ComPortMessage.SaveDataBytes(DataBytes);
end;


//////////////////////TMController////////////////////////////////////////////
constructor TMController.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMController.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;

procedure TMController.Init;
var
  Conveyor : TMConveyor;
  i : integer;
begin
  FSignalMode := smDisabled;
  for i := 0 to 4 do
    begin
      Conveyor := TMConveyor.Create;

      Conveyor.FNumber     := i + 1;
      Conveyor.FWorkMode   := cwmOverlocking;

      AddItem(Conveyor);
    end;
end;


function TMController.GetItem(ItemIndex : integer) : TMConveyor;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMConveyor;
end;

function TMController.GetItem(ItemTitle : string) : TMConveyor;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMController.AddItem(Item : TMConveyor) : TMConveyor;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

   if Items.AddObject(IntToStr(Item.FNumber), Item) >= 0
    then Result := Item;
end;


function TMController.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMConveyor;
begin
  Result := FALSE;
  Item := GetItem(ItemIndex);
  if Assigned(Item) then
    begin
      Items.Delete(ItemIndex);
      Item.Free;
      Result := TRUE;
    end;
end;

function TMController.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


procedure TMController.Reset;
var
  i : integer;
  Item : TMConveyor;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMConveyor;
    if Assigned(Item)
      then Item.Free;
  end;

  FSignalMode := smNone;

  Items.Clear;
end;

procedure TMController.DeHighLight();
var
  i, count : integer;
  Conveyor : TMConveyor;
begin
  count := GetCount;

  for i := 0 to count - 1 do
    begin
      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      Conveyor.DeHighLight;
    end;
end;


function TMController.GetCount : integer;
begin
  Result := Items.Count;
end;


function TMController.CheckTemperatureRanges : boolean;
var
  i, j,
  conveyor_number,
  section_number,
  conveyor_count,
  section_count : integer;

  check_average : boolean;

  Conveyor : TMConveyor;
  Section  : TMColumn;

  defailure_array : array of integer;
begin
  DeHighLight; //Убираем все выделения на графике

  conveyor_count := GetCount;

  for i := 0 to conveyor_count - 1 do
    begin
      conveyor_number := i + 1;

      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      for j := 1 to 10 do
        begin
          section_number := j;

          check_average := ApplicationTempBufferValues.CheckAverage(section_number, conveyor_number);
          if not check_average
            then
              begin
                Conveyor.FailureSection(section_number);

                if Conveyor.WorkMode = cwmWork
                  then
                    begin
                      Conveyor.HighLight(section_number);
                      if SignalMode = smDisabled
                        then
                          begin
                            SignalMode := smEnabled; //Включение сигнализации
                            ApplicationEventLog.WriteLog(elSignalOn, 'Floor: ' + IntToStr(conveyor_number) + '; section: ' + IntToStr(section_number));
                          end;

                    end;
              end;
        end;
    end;

  conveyor_count := GetCount;

  for i := 0 to conveyor_count - 1 do
    begin
      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      if not Conveyor.IsFailure
        then Continue;

      conveyor_number := Conveyor.Number;

      section_count := Conveyor.OutOfRangeSections.GetCount;

      SetLength(defailure_array, 0);

      for j := 0 to section_count - 1 do
        begin
          Section := Conveyor.OutOfRangeSections.GetItem(j);

          if not Assigned(Section)
            then Continue;

          section_number := Section.ColumnIndex;

          check_average := ApplicationTempBufferValues.CheckAverage(section_number, conveyor_number);

          if check_average //сохраняем в массив номера всех секций, в которых уже нет ошибок
            then           //если сразу удалять, то будет изменяться размер текущего цикла и куча ошибок
              begin
                SetLength(defailure_array, Length(defailure_array) + 1);
                defailure_array[Length(defailure_array) - 1] := section_number;
              end;
        end;

        for j := 0 to Length(defailure_array) - 1 do
          begin
            section_number := defailure_array[j];

            Conveyor.DeFailureSection(section_number);
          end;

        if Conveyor.WorkMode = cwmWork
            then
              begin
                if (not Conveyor.IsFailure) and (SignalMode = smEnabled)
                  then
                    begin
                      SignalMode := smDisabled; //Здесь сигнализация должна выключаться
                      ApplicationEventLog.WriteLog(elSignalOff);
                    end;
              end;
    end;
end;


end.
