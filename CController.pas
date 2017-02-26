unit CController;

interface

uses CIncomingComPortMessage, COutgoingComPortMessage, Classes, CRows;

const CONVEYORS_COUNT = 5;

type
  TypeConveyorWorkMode = (cwmNone,
                          cwmOverlocking,
                          cwmWork);

  TypeConveyorSignalMode = (csmNone,
                            csmEnabled,
                            csmDisabled);

type
  TMConveyor = class
    constructor Create;
    destructor Destroy; override;
  private
    FNumber     : integer;
    FWorkMode   : TypeConveyorWorkMode;
    FSignalMode : TypeConveyorSignalMode;

    FOutOfRangeSections : TMRow;

    procedure Reset;
    function IsFailure : boolean;
    function GetWorkModeString : string;

  public
    function SaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)   : boolean; virtual; abstract;
    function LoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean; virtual; abstract;

    procedure HighLight(ASectionNumber : integer);

    procedure DeHighLight(); overload;
    procedure DeHighLight(ASectionNumber : integer); overload;

    procedure FailureSection(ASectionNumber : integer);
    procedure DeFailureSection(ASectionNumber : integer);

  public
    property Number     : integer                read FNumber;
    property WorkMode   : TypeConveyorWorkMode   read FWorkMode    write FWorkMode;
    property SignalMode : TypeConveyorSignalMode read FSignalMode  write FSignalMode;

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
    procedure Init;

  private
    Items : TStringList;

    procedure Reset;
    procedure DeHighLight();
  end;

implementation

uses SysUtils, CProgramSettings, LApplicationGlobals, CTempValuesBuffer, Types,
     FMain;

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
  FSignalMode := csmNone;

  FOutOfRangeSections.Clear;
end;

function TMConveyor.IsFailure : boolean;
begin
  FOutOfRangeSections.GetCount > 0;
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
  for i := 0 to 4 do
    begin
      Conveyor := TMConveyor.Create;

      Conveyor.FNumber     := i + 1;
      Conveyor.FWorkMode   := cwmOverlocking;
      Conveyor.FSignalMode := csmDisabled;

      Conveyor := AddItem(Conveyor);
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
  count : integer;

  range_min, range_max : single;

  TempValueBuffer : TMTempBufferValue;

  Conveyor : TMConveyor;
  Section  : TMColumn;
begin
  DeHighLight; //Убираем все выделения на графике

  count := ApplicationTempBufferValues.GetCount; //Количество датчиков в буфере

  for i := 0 to count - 1 do
    begin
      TempValueBuffer := ApplicationTempBufferValues.GetItem(i);

      if not Assigned(TempValueBuffer)
        then Continue;

      section_number  := TempValueBuffer.SectionNumber;
      conveyor_number := TempValueBuffer.ConveyorNumber;

      Conveyor := GetItem(IntToStr(conveyor_number));

      if not Assigned(Conveyor)
        then Exit;

      range_min := GetSectionYMinValue(section_number);
      range_max := GetSectionYMaxValue(section_number);

      if (TempValueBuffer.TempValue > range_max) or
         (TempValueBuffer.TempValue < range_min)
        then
          begin
            Conveyor.FailureSection(section_number);

            if Conveyor.WorkMode = cwmWork
              then
                begin
                  Conveyor.HighLight(section_number);
                  if Conveyor.SignalMode = csmDisabled
                    then
                      begin
                        Conveyor.SignalMode := csmEnabled; //Здесь должно инициироваться включение сигнализации
                        FormMain.WriteLog('Включена сигнализация. Конвейер ' + IntToStr(Conveyor.Number));
                      end;
                end;
          end;
    end;


  //Если
  count := GetCount;

  for i := 0 to count - 1 do
    begin
      Conveyor := GetItem(i);

      if not Assigned(Conveyor)
        then Continue;

      if not Conveyor.IsFailure
        then Continue;

      conveyor_number := Conveyor.Number;

      for j := 0 to Conveyor.OutOfRangeSections.GetCount - 1 do
        begin
          Section := Conveyor.OutOfRangeSections.GetItem(j);

          if not Assigned(Section)
            then Continue;

          section_number := Section.ColumnIndex;

          if ApplicationTempBufferValues.CheckAverage(section_number, conveyor_number)
            then Conveyor.DeFailureSection(section_number);

          if not Conveyor.IsFailure
            then
              begin
                Conveyor.SignalMode := csmDisabled; //Здесь сигнализация должна выключаться
                FormMain.WriteLog('Выключена сигнализация. Конвейер ' + IntToStr(Conveyor.Number));
              end;
        end;
    end;                    
end;


end.
