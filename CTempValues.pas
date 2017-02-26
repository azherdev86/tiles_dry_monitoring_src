unit CTempValues;

interface

uses CTemplateEntity, Classes, CTableRecords;

type
  TMTempValue = class(TMTemplateEntity)
    constructor Create;
    destructor Destroy; override;

  private
    FTempValueId : variant;

    FSensorId  : variant;
    FTempValue : single;
    FTempTime  : TDateTime;

    FRecordSensors : TMTableRecord;

  protected
    procedure GetTableRecordData(); override;
    procedure SetTableRecordData(); override;
    procedure SetPKFieldValue(Value : variant); override;

  public
    property SensorId  : variant   read FSensorId  write FSensorId;
    property TempValue : single    read FTempValue write FTempValue;
    property TempTime  : TDateTime read FTempTime  write FTempTime;

    property RecordSensors : TMTableRecord read FRecordSensors;
end;

type
  TMTempValuesList = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMTempValue;

    function AddItem(Item : TMTempValue) : TMTempValue;
    function DeleteItem(ItemIndex : integer) : boolean;

    function GetCount : integer;

    function SaveToDataBase() : integer;

    procedure Clear;

  private
    Items : TStringList;

    procedure Reset;
  end;

  function TempToFloat(ATemp : Word) : single;


implementation

uses SysUtils;

function TempToFloat(ATemp : Word) : single;
begin
  Result := ATemp/10;
end;

//////////////////////////CTempValue/////////////////////////

constructor TMTempValue.Create;
begin
  FTableName := 'TempValues';

  FRecordSensors := TMTableRecord.Create('Sensors');

  inherited;
end;

destructor TMTempValue.Destroy;
begin
  FRecordSensors.Free;

  inherited;
end;


procedure TMTempValue.SetPKFieldValue(Value : variant);
begin
  FTempValueId := Value;
end;

procedure TMTempValue.GetTableRecordData();
begin
  with FTableRecord do
  begin
    FTempValueId := FieldByName['TempValueId'].Value;

    FSensorId    := FieldByName['SensorId'].Value;
    FTempValue   := FieldByName['TempValue'].Value;
    FTempTime    := FieldByName['TempTime'].Value;
  end;
end;

procedure TMTempValue.SetTableRecordData();
begin
  with FTableRecord do
  begin
    FieldByName['TempValueId'].Value := FTempValueId;

    FieldByName['SensorId'].Value  := FSensorId;
    FieldByName['TempValue'].Value := FTempValue;
    FieldByName['TempTime'].Value  := FTempTime;
  end;
end;

//////////////TMTempValuesList//////////////////////////////////
constructor TMTempValuesList.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMTempValuesList.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMTempValuesList.GetItem(ItemIndex : integer) : TMTempValue;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMTempValue;
end;

function TMTempValuesList.AddItem(Item : TMTempValue) : TMTempValue;
var
  Index : integer;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  Index := GetCount;

  if Items.AddObject(IntToStr(Index), Item) >= 0
    then Result := Item;
end;


function TMTempValuesList.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMTempValue;
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

procedure TMTempValuesList.Reset;
var
  i : integer;
  Item : TMTempValue;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMTempValue;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;
end;

function TMTempValuesList.GetCount : integer;
begin
  Result := Items.Count;
end;


procedure TMTempValuesList.Clear;
begin
  Reset;
end;


function TMTempValuesList.SaveToDataBase() : integer;
var
  i, count: integer;

  Item : TMTempValue;
begin
  Result := 0;

  count := GetCount;

  for i := 0 to count - 1 do
    begin
      Item := GetItem(i);

      if not Assigned(Item) 
        then Continue;

      if (Item.SaveToDataBase > 0)
        then INC(Result);
    end;
end;


end.
