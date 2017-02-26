unit CController;

interface

uses CIncomingComPortMessage, COutgoingComPortMessage, Classes, CRows;



type
  TypeConveyorWorkMode = (cwmNone,
                          cwmOverlocking,
                          cwmWork);

  TypeConveyorSignalMode = (csmNone,
                            csmEnabled,
                            csmDisabled);

type
  TMConveyor = class
    private
      FNumber     : integer;
      FWorkMode   : TypeConveyorWorkMode;
      FSignalMode : TypeConveyorSignalMode;

      FOutOfRangeRow : TMRow;


    public
      function SaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)   : boolean; virtual; abstract;
      function LoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean; virtual; abstract;

    public
      property Number : integer read FNumber;
      property WorkMode : TypeConveyorWorkMode read FWorkMode write FWorkMode;
      property SignalMode : TypeConveyorSignalMode read FSignalMode;
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
  end;

implementation

uses SysUtils, CProgramSettings, LApplicationGlobals, CTempValuesBuffer, Types;

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

      if not Assigned(Conveyor)
        then Conveyor := nil;
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


function TMController.GetCount : integer;
begin
  Result := Items.Count;
end;


function TMController.CheckTemperatureRanges : boolean;
var
  i, count,
  conveyor_number,
  section_number : integer;

  range_min, range_max : single;

  TempValueBuffer : TMTempBufferValue;

  HighLightedCell : TPoint;
begin
  count := ApplicationTempBufferValues.GetCount;

  for i := 0 to count - 1 do
    begin
      TempValueBuffer := ApplicationTempBufferValues.GetItem(i);

      if not Assigned(TempValueBuffer)
        then Continue;

      section_number  := TempValueBuffer.SectionNumber;
      conveyor_number := TempValueBuffer.ConveyorNumber;

      range_min := GetSectionYMinValue(section_number);
      range_max := GetSectionYMaxValue(section_number);

      HighLightedCell := Point(section_number - 1, conveyor_number - 1);

      if (TempValueBuffer.TempValue > range_max) or
         (TempValueBuffer.TempValue < range_min)
        then ApplicationGraph.HighLightCell(HighLightedCell);
    end;

end;


end.
