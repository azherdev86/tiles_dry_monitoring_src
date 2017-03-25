unit CSensorValueCounters;

interface

uses Classes;

type
  TMSensorValueCounter = class
    constructor Create;
    destructor Destroy; override;

  private
    FSensorId : integer;
    FValuesArray : array of single;

    procedure Reset;
    function GetIntegrationValue() : single;

  public
    property SensorId : integer read FSensorId write FSensorId;
    property IntegrationValue : single read GetIntegrationValue;

  public
    procedure AddValue(AValue : single);
  end;

type
  TMSensorValueCounters = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMSensorValueCounter;  overload;
    function GetItem(ItemTitle : string) : TMSensorValueCounter; overload;

    function AddItem(Item : TMSensorValueCounter) : TMSensorValueCounter;
    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

    procedure Clear;

  private
    FItems : TStringList;

    procedure Reset;
  end;

implementation

uses SysUtils, LApplicationGlobals;

//////////////////TMSensorValueCounter////////////////
constructor TMSensorValueCounter.Create;
begin
  inherited;

  Reset;
end;

destructor TMSensorValueCounter.Destroy;
begin
  Reset;

  inherited;
end;

procedure TMSensorValueCounter.Reset;
begin
  FSensorId := 0;

  SetLength(FValuesArray, 0);
end;

procedure TMSensorValueCounter.AddValue(AValue : single);
var
  integration_count,
  array_count,
  i : integer;
begin
  integration_count := ApplicationProgramSettings.UserSettings.IntegrationTempValueCount;

  array_count := Length(FValuesArray);

  if array_count < integration_count
    then SetLength(FValuesArray, array_count + 1);

  array_count := Length(FValuesArray);

  for i := (array_count - 1) downto 1 do
    begin
      if array_count = 1
        then Break;

      FValuesArray[i] := FValuesArray[i - 1];
     end;

  FValuesArray[0] := AValue;
end;

function TMSensorValueCounter.GetIntegrationValue() : single;
var
  i, count : integer;

  tmp_value : single;
begin
  count := Length(FValuesArray);

  tmp_value := 0;

  for i := 0 to count - 1 do
    tmp_value := tmp_value + FValuesArray[i];

  Result := tmp_value/count;
end;

//////////////////TMSensorValueCounterS////////////////
constructor TMSensorValueCounters.Create;
begin
  FItems := TStringList.Create;

  Reset;
end;


destructor TMSensorValueCounters.Destroy;
begin
  Reset;
  FItems.Free;

  inherited;
end;


function TMSensorValueCounters.GetItem(ItemIndex : integer) : TMSensorValueCounter;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := FItems.Objects[ItemIndex] as TMSensorValueCounter;
end;

function TMSensorValueCounters.GetItem(ItemTitle : string) : TMSensorValueCounter;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := FItems.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;

function TMSensorValueCounters.AddItem(Item : TMSensorValueCounter) : TMSensorValueCounter;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if FItems.AddObject(IntToStr(Item.SensorId), Item) >= 0
    then Result := Item;
end;

function TMSensorValueCounters.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMSensorValueCounter;
begin
  Result := FALSE;
  Item := GetItem(ItemIndex);
  if Assigned(Item) then
    begin
      FItems.Delete(ItemIndex);
      Item.Free;
      Result := TRUE;
    end;
end;

function TMSensorValueCounters.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := FItems.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;

procedure TMSensorValueCounters.Reset;
var
  i, count : integer;
  Item : TMSensorValueCounter;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    Item := FItems.Objects[i] as TMSensorValueCounter;
    if Assigned(Item)
      then Item.Free;
  end;

  FItems.Clear;
end;

function TMSensorValueCounters.GetCount : integer;
begin
  Result := FItems.Count;
end;

procedure TMSensorValueCounters.Clear;
begin
  Reset;
end;



end.
