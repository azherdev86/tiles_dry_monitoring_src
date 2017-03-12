unit CController;

interface

uses CIncomingComPortMessage, COutgoingComPortMessage, Classes, CRows;

const
  CONVEYORS_COUNT = 5;

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
    function getProblem : boolean;
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
    procedure CheckTemperatureRanges;// Основной метод
    ////////////////////////////////////////////////////////////

    procedure GenerateCheckSignalModeMessage(); //Отправка сообщения на запрос статуса сигнализации
    procedure GenerateSetSignalModeMessage(ASignalMode : TypeSignalMode); //Отправка сообщения на включение/выключение сигнализации

    function CheckSignalModeLoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;
    function SetSignalModeLoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;

    procedure Init;
  private
    Items : TStringList;

    FSignalMode : TypeSignalMode;
    FSignalEnabledForCurrentProblem : boolean; //флаг, который используется для того, чтобы сигнализация
                                               //не включалась повторно после её выключения в рамках
                                               //текущей проблемы

    procedure Reset;
    procedure DeHighLight();

    function getProblem : boolean;

    function ManageSignalSaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage) : boolean; //управление сигналом
    function ModeSignalSaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)  : boolean; //запрос состояния сигнала

  public
    property SignalMode : TypeSignalMode read FSignalMode;    //проверка статуса сигнализации

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

function TMConveyor.getProblem : boolean;
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
  FSignalEnabledForCurrentProblem := False;

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


procedure TMController.CheckTemperatureRanges;
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
    begin //Проверяем для каждого конвейера
      conveyor_number := i + 1;

      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      for j := 1 to 10 do
        begin //Для каждой секции в конвейере
          section_number := j;

          //Проверяем. что среднее значение по любой из пар не выходит за пределы диапазона
          check_average := ApplicationTempBufferValues.CheckAverage(section_number, conveyor_number);
          if not check_average
            then //Если выходит за границы
              begin
                Conveyor.FailureSection(section_number); //Фиксируем ошибку в секции

                if Conveyor.WorkMode = cwmWork
                  then   //Если рабочий режим
                    begin
                      Conveyor.HighLight(section_number);
                      if FSignalMode = smDisabled  //Если сигнализация была выключена, то включаем
                        then
                          begin
                            if not FSignalEnabledForCurrentProblem
                              then GenerateSetSignalModeMessage(smEnabled);//Отправка запроса на включение. Статус изменяется при приеме

                            ApplicationEventLog.WriteLog(elTempRangeOut, 'Floor: ' + IntToStr(conveyor_number) + '; section: ' + IntToStr(section_number));
                          end;

                    end;
              end;
        end;
    end;

  conveyor_count := GetCount;

  for i := 0 to conveyor_count - 1 do //Теперь нужно проверить секции, которые помечены ошибками
    begin //Ошибки могут быть как полученные на этом шаге (в коде чуть выше), так и полученные на предыдущих шагах
      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      if not Conveyor.getProblem //Если конвейер не находится в режиме ошибки
        then Continue; //то переходим к следующему

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
          begin //Снимаем ошибки в определенных в предыдущем цикле секциях
            section_number := defailure_array[j];

            Conveyor.DeFailureSection(section_number);
          end;

        if (not getProblem) and ((FSignalMode = smEnabled) or (FSignalEnabledForCurrentProblem = True))
          then  //Если ошибок нет, а сигнализация при это включена
            begin
              if (FSignalMode = smEnabled)
                then GenerateSetSignalModeMessage(smDisabled) //Добавляем сообщение о выключении сигнализации в очередь на отправку
                else FSignalEnabledForCurrentProblem := False; //Если сигнализация и так отключена, обнуляем флаг

              ApplicationEventLog.WriteLog(elTempRangeIn);

            end;
    end;

  //Этот кусок нужен для того, чтобы выключить включенную сигнализацию
  //Для тех случаев, когда сигнализация была включена не из программы
  //Т.е. сигнализация была запущена на момент запуска программы
  if (not getProblem) and (FSignalMode = smEnabled)
    then GenerateSetSignalModeMessage(smDisabled) //Добавляем сообщение о выключении сигнализации в очередь на отправку
end;

procedure TMController.GenerateCheckSignalModeMessage();
var
  ComPortMessage : TMOutgoingComportMessage;
begin
  ComPortMessage := TMOutgoingComportMessage.Create;

  if not ModeSignalSaveToComPortMessage(ComPortMessage)
    then Exit;

  ApplicationComPortOutgoingMessages.AddItem(ComPortMessage);
end;


procedure TMController.GenerateSetSignalModeMessage(ASignalMode : TypeSignalMode);
var
  ComPortMessage : TMOutgoingComportMessage;
begin
  ComPortMessage := TMOutgoingComportMessage.Create;

  FSignalMode := ASignalMode;

  if not ManageSignalSaveToComPortMessage(ComPortMessage)
    then Exit;

  ApplicationComPortOutgoingMessages.AddItem(ComPortMessage);
end;

function TMController.getProblem : boolean;
var
  i, count : integer;
  Conveyor : TMConveyor;
begin
  Result := False;

  count := GetCount;

  for i := 0 to count - 1 do
    begin
      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      if not (Conveyor.WorkMode = cwmWork)
        then Continue; //проверяем только конвейеры в рабочем режиме

      if Conveyor.getProblem
        then
          begin
            Result := TRUE;
            Break;
          end;
    end;
end;

function TMController.ManageSignalSaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)   : boolean;
var
  DataBytes : TDynamicByteArray;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  SetLength(DataBytes, 2);

  DataBytes[0] := $00;

  case FSignalMode of
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

function TMController.ModeSignalSaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)  : boolean; //запрос состояния сигнала
var
  DataBytes : TDynamicByteArray;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  SetLength(DataBytes, 2);

  DataBytes[0] := $00;
  DataBytes[1] := $01;

  ComPortMessage.LoadDataBytes(DataBytes);

  ComPortMessage.DeviceId  := $02;
  ComPortMessage.CommandId := $03;

  ComPortMessage.MSBRegisterAddr := $00;
  ComPortMessage.LSBRegisterAddr := $00;

  ComPortMessage.Priority  := mpHigh;

  ComPortMessage.CreationTime := Now;

  Result := True;
end;

function TMController.CheckSignalModeLoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;
var
  DataBytes : TDynamicByteArray;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  if ComPortMessage.DeviceId <> $02
    then Exit;

  ComPortMessage.SaveDataBytes(DataBytes);

  if Length(DataBytes) = 0
    then Exit;

  if DataBytes[0] = $00
    then FSignalMode := smDisabled
    else FSignalMode := smEnabled;

  Result := True;
end;


function TMController.SetSignalModeLoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;
var
  DataBytes : TDynamicByteArray;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  if ComPortMessage.DeviceId <> $02
    then Exit;

  ComPortMessage.SaveDataBytes(DataBytes);

  if Length(DataBytes) = 0
    then Exit;

  if DataBytes[1] = $00
    then
      begin
        ApplicationEventLog.WriteLog(elSignalOff);
        FSignalMode := smDisabled;
        if not getProblem
          then FSignalEnabledForCurrentProblem := False;
      end
    else
      begin
        ApplicationEventLog.WriteLog(elSignalOn);
        FSignalMode := smEnabled;
        FSignalEnabledForCurrentProblem := True;
      end;

  Result := True;
end;



end.
