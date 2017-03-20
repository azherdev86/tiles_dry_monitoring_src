unit CBoxes;

interface

uses CTemplateEntity, COutgoingComPortMessage, CIncomingComPortMessage,
     CBasicComPortMessage, CTableRecords, CTempValues, Classes,
     CSensorValueCounters;

const
  BOXES_COUNT = 20;

type
  TMBox = class
    constructor Create(ABoxNumber : Byte);
    destructor Destroy; override;

  private
    FBoxNumber : Byte;

    FRecordSensors : TMTableRecord;
    FTempValuesList : TMTempValuesList;
    FSensorValueCounters : TMSensorValueCounters;

    procedure Reset();

    function GetSensorId(AValueIndex : integer) : integer;
    function SaveToTempBufferValues() : boolean;
    function CalculateIntegrationValue(ASensorId : integer; AValue : Word) : single;
  public
    function SaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage)   : boolean;
    function LoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;

  public
    property BoxNumber : byte read FBoxNumber;
end;

type
  TMBoxesList = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMBox; overload;
    function GetItem(ItemName  : string) : TMBox;  overload;

    function AddItem(Item : TMBox) : TMBox;
    function DeleteItem(ItemIndex : integer) : boolean;

    function GetCount : integer;

    procedure Clear;

    procedure Init;

  private
    Items : TStringList;
    FBoxesCount : integer;

    procedure Reset;
  end;


implementation

uses SysUtils, LApplicationGlobals, CTempValuesBuffer, FMain;

//////////////////////////////TMBox/////////////////////////////////////////////

constructor TMBox.Create(ABoxNumber : Byte);
begin
  FRecordSensors  := TMTableRecord.Create('Sensors');

  FTempValuesList := TMTempValuesList.Create;

  FSensorValueCounters := TMSensorValueCounters.Create;

  Reset();

  FBoxNumber := ABoxNumber;
end;

destructor TMBox.Destroy;
begin
  Reset;

  FRecordSensors.Free;
  FTempValuesList.Free;
  FSensorValueCounters.Free;

  inherited;
end;


function TMBox.GetSensorId(AValueIndex : integer) : integer;
var
  SensorPosition,
  SensorSide : string;

  ConveyorNumber : integer;
begin
  Result := 0;

  SensorPosition := '';
  ConveyorNumber := 0;

  if (FBoxNumber mod 2) = 0
    then SensorSide := 'Right'
    else SensorSide := 'Left';

  if (AValueIndex mod 2) = 0
    then SensorPosition := SensorSide + 'Bottom'
    else SensorPosition := SensorSide + 'Top';

  case AValueIndex of
    0, 1 : ConveyorNumber := 1;
    2, 3 : ConveyorNumber := 2;
    4, 5 : ConveyorNumber := 3;
    6, 7 : ConveyorNumber := 4;
    8, 9 : ConveyorNumber := 5;
  end;

  if FRecordSensors.LoadRecordsAND('BoxNumber', FBoxNumber,
                                   'ConveyorNumber', ConveyorNumber,
                                   'SensorPosition', SensorPosition) > 0
    then Result := FRecordSensors.PKFieldValue;
end;


function TMBox.SaveToComPortMessage(ComPortMessage : TMOutgoingComportMessage) : boolean;
var
  DataBytes : TDynamicByteArray;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  SetLength(DataBytes, 2);

  DataBytes[0] := $00;
  DataBytes[1] := $0A;

  ComPortMessage.LoadDataBytes(DataBytes);
  ComPortMessage.DeviceId  := FBoxNumber;
//  ComPortMessage.DebugDeviceId := FBoxNumber;
  ComPortMessage.CommandId := $04;
  ComPortMessage.MSBRegisterAddr := $03;
  ComPortMessage.LSBRegisterAddr := $E8;

  ComPortMessage.Priority  := mpNormal;

  ComPortMessage.CreationTime := Now;

  Result := True;
end;

function TMBox.LoadFromComPortMessage(ComPortMessage : TMIncomingComportMessage) : boolean;
var
  len, i, value_index : integer;
  DataBytes : TDynamicByteArray;
  LSB, MSB : Byte;
  Value : Word;

  TempValue: TMTempValue;
  MessageTime : TDateTime;
begin
  Result := False;

  if not Assigned(ComPortMessage)
    then Exit;

  if ComPortMessage.DeviceId <> FBoxNumber
    then Exit;

  MessageTime := ComPortMessage.RecievedTime;

  ComPortMessage.SaveDataBytes(DataBytes);

  len := Length(DataBytes);

  i := 0;
  value_index := 0;

  FTempValuesList.Clear;

  while i < len do
  begin
    MSB := DataBytes[i];
    LSB := DataBytes[i + 1];

    Value := MSB shl 8 or LSB;

    TempValue := TMTempValue.Create;

    TempValue.SensorId  := GetSensorId(value_index);
    TempValue.TempValue := CalculateIntegrationValue(TempValue.SensorId, Value);
    TempValue.TempTime  := MessageTime;

    if (TempValue.RecordSensors.LoadRecord(TempValue.SensorId) = 0)
      then
        begin
          TempValue.Free;
          Continue;
        end;

    TempValue := FTempValuesList.AddItem(TempValue);

    if not Assigned(TempValue)
      then TempValue.Free;

    i := i + 2;
    value_index := value_index + 1;
  end;

  FTempValuesList.SaveToDataBase;

  if SaveToTempBufferValues
    then Result := TRUE
    else Result := FALSE;
end;

function TMBox.CalculateIntegrationValue(ASensorId : integer; AValue : Word) : single;
var
  value : single;
  SensorValueCounter : TMSensorValueCounter;
begin
  value := TempToFloat(AValue);

  SensorValueCounter := FSensorValueCounters.GetItem(IntToStr(ASensorId));

  if not Assigned(SensorValueCounter)
    then
      begin
        SensorValueCounter := TMSensorValueCounter.Create;

        SensorValueCounter.SensorId := ASensorId;

        SensorValueCounter := FSensorValueCounters.AddItem(SensorValueCounter);
      end;

  SensorValueCounter.AddValue(value);

  Result := SensorValueCounter.IntegrationValue;
end;


function TMBox.SaveToTempBufferValues() : boolean;
var
  i, count : integer;

  TempBufferValue : TMTempBufferValue;
  TempValue : TMTempValue;
  RecordSensors   : TMTableRecord;
begin
  Result := TRUE;

  count := FTempValuesList.GetCount;

  for i := 0 to count - 1 do
    begin
      TempValue := FTempValuesList.GetItem(i);

      if not Assigned(TempValue)
        then Continue;

      RecordSensors := TempValue.RecordSensors;

      if not Assigned(RecordSensors)
        then Continue;

      TempBufferValue := TMTempBufferValue.Create;

      TempBufferValue.SensorId   := TempValue.SensorId;
      TempBufferValue.TempValue  := TempValue.TempValue;
      TempBufferValue.TempTime   := TempValue.TempTime;

      TempBufferValue.ConveyorNumber := RecordSensors.FieldByName['ConveyorNumber'].AsInteger;
      TempBufferValue.SectionNumber  := RecordSensors.FieldByName['SectionNumber'].AsInteger;
      TempBufferValue.BoxNumber      := RecordSensors.FieldByName['BoxNumber'].AsInteger;
      TempBufferValue.SensorPosition := RecordSensors.FieldByName['SensorPosition'].AsString;

      TempBufferValue := ApplicationTempBufferValues.AddItem(TempBufferValue);

      if not Assigned(TempBufferValue)
        then TempBufferValue.Free;
    end;
end;

procedure TMBox.Reset();
begin
  FBoxNumber := 0;

  FTempValuesList.Clear;
  FSensorValueCounters.Clear;
end;

//////////////TMBoxesList//////////////////////////////////
constructor TMBoxesList.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMBoxesList.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMBoxesList.GetItem(ItemIndex : integer) : TMBox;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMBox;
end;

function TMBoxesList.GetItem(ItemName : string) : TMBox;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemName);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMBoxesList.AddItem(Item : TMBox) : TMBox;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(IntToStr(Item.FBoxNumber), Item) >= 0
    then Result := Item;
end;


function TMBoxesList.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMBox;
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

procedure TMBoxesList.Reset;
var
  i : integer;
  Item : TMBox;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMBox;
    if Assigned(Item)
      then Item.Free;
  end;

  FBoxesCount := 0;
  Items.Clear;
end;

function TMBoxesList.GetCount : integer;
begin
  Result := Items.Count;
end;


procedure TMBoxesList.Clear;
begin
  Reset;
end;

procedure TMBoxesList.Init;
var
  i : integer;

  Item : TMBox;

  BoxesNumbers : array [0..19] of Byte;
begin
  Reset;

  FBoxesCount := BOXES_COUNT;

  BoxesNumbers[0] := 11;
  BoxesNumbers[1] := 12;
  BoxesNumbers[2] := 21;
  BoxesNumbers[3] := 22;
  BoxesNumbers[4] := 31;
  BoxesNumbers[5] := 32;
  BoxesNumbers[6] := 41;
  BoxesNumbers[7] := 42;
  BoxesNumbers[8] := 51;
  BoxesNumbers[9] := 52;
  BoxesNumbers[10] := 61;
  BoxesNumbers[11] := 62;
  BoxesNumbers[12] := 71;
  BoxesNumbers[13] := 72;
  BoxesNumbers[14] := 81;
  BoxesNumbers[15] := 82;
  BoxesNumbers[16] := 91;
  BoxesNumbers[17] := 92;
  BoxesNumbers[18] := 101;
  BoxesNumbers[19] := 102;

  for i := 0 to FBoxesCount - 1 do
  begin
    Item := TMBox.Create(BoxesNumbers[i]);

    Item := AddItem(Item);

    if not Assigned(Item)
      then Item.Free;
  end;
end;


end.
