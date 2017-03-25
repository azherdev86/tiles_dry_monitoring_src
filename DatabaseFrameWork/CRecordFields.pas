unit CRecordFields;

interface

uses Classes, CFields, Variants;


type
  TMRecordField = class(TMField)
    constructor Create(); overload; override;
    constructor Create(Field : TMField); overload; override;
    destructor Destroy(); override;

  private
    FValue : variant;
  private
    function GetValueAsInteger  : integer;
    function GetValueAsVariant  : variant;
    function GetValueAsString   : string;
    function GetValueAsDateTime : TDateTime;
    function GetValueAsBoolean  : boolean;
    function GetValueAsFloat    : single;

    procedure SetValue(V : Variant);

    procedure LoadFromField(Field : TMField);

    function DateToMySQLString(Date : TDateTime) : string;

    function TimeToMySQLString(Date : TDateTime) : string;

    function DateTimeToFirebirdString(const Date: TDateTime): string;
    function DateTimeToMySQLString(const  Date : TDateTime) : string;

    function TimeToFirebirdString(const Date: TDateTime): string;
    function DateToFirebirdString(const Date: TDateTime): string;

  protected
    function GetValueString() : string; virtual;


  public
    //Свойства для доступа к данным поля RecordField
    property AsVariant  : variant   read GetValueAsVariant;
    property AsInteger  : integer   read GetValueAsInteger;
    property AsString   : string    read GetValueAsString;
    property AsDateTime : TDateTime read GetValueAsDateTime;
    property AsBoolean  : Boolean   read GetValueAsBoolean;
    property AsFloat    : single    read GetValueAsFloat;

    property Value : variant read FValue write SetValue;
    property ValueString : string read GetValueString;

  public
    procedure Reset; override;
    function CopyToRecordField : TMRecordField;
    procedure CopyFromRecordField(RecordField : TMRecordField);
end;

type
  TMRecordFields = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMRecordField; overload;
    function GetItem(ItemName : string) : TMRecordField; overload;

    function AddItem(Item : TMRecordField) : TMRecordField;
    function AddItems(RecordFields : TMRecordFields) : integer;

    function DeleteItem(ItemName : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;
  end;

implementation


uses DB, SysUtils, DateUtils, LApplicationGlobals;

/////////////////////////TMRecordField.////////////////////////
constructor TMRecordField.Create(Field : TMField);
begin
  Create;
  LoadFromField(Field);
end;


constructor TMRecordField.Create();
begin
  inherited;
  Reset;
end;


destructor TMRecordField.Destroy();
begin
  Reset;
  inherited;
end;

procedure TMRecordField.Reset;
begin
  FValue := null;
  inherited;
end;

function TMRecordField.CopyToRecordField : TMRecordField;
var
  NewField : TMRecordField;
begin
  NewField := TMRecordField.Create;

  NewField.FValue := FValue;
  NewField.FieldName := FieldName;
  NewField.FieldType := FieldType;

  Result := NewField;
end;

procedure TMRecordField.CopyFromRecordField(RecordField : TMRecordField);
begin
  if not Assigned(RecordField)
    then Exit;

  FValue := RecordField.FValue;
  CopyFromField(RecordField as TMField);
end;


function TMRecordField.GetValueAsInteger  : integer;
begin
  Result := FValue;
end;

function TMRecordField.GetValueAsVariant  : variant;
begin
  Result := FValue;
end;

function TMRecordField.GetValueAsString   : string;
begin
  Result := VarToStr(FValue);
end;

function TMRecordField.GetValueAsDateTime : TDateTime;
begin
  Result := VarToDateTime(FValue);
end;

function TMRecordField.GetValueAsFloat : single;
begin
  Result := FValue;
end;


function TMRecordField.GetValueAsBoolean  : boolean;
begin
  if FValue = 0
    then Result := FALSE
    else Result := TRUE;
end;

procedure TMRecordField.SetValue(V : Variant);
begin
  FValue := V;
end;

function TMRecordField.GetValueString() : string;
begin
  Result := '';

  if VarIsEmpty(Value) or (Value = null)
    then
      begin
        Result := 'null';
        Exit;
      end;

  case FieldType of
    ftUnknown  : Result := Value;
    ftString   : Result := '''' + Value + '''';
    ftInteger  : Result := VarToStr(Value);
    ftVariant  : Result := Value;
    ftFloat    :
      begin
        Result := FloatToStrF(Value, ffFixed, 4, 1, ApplicationFormatSettings);
        Result := StringReplace(Result, ',', '.', [rfReplaceAll])
      end;

    //Для всех типов связанных с временем 0 трактуется - как незаполненно NULL
    ftDate     :
      begin
        if Value = null
          then Result := 'NULL'
          //Учитывать какой тип бд
          else Result := '''' + DateToFirebirdString(Value)     + '''';
      end;

    ftTime     :
      begin
        if Value = null
          then Result := 'NULL'
          //Учитывать какой тип бд
          else Result := '''' + TimeToFirebirdString(Value)     + '''';
      end;

    ftDateTime :
      begin
        if Value = null
          then Result := 'NULL'
          //Учитывать какой тип бд
          else Result := '''' + DateTimeToFirebirdString(Value) + '''';
      end;

    ftBoolean  :
      begin
        if Value = False
          then Result := '0'
          else Result := '1';
        end;
  end;
end;


procedure TMRecordField.LoadFromField(Field : TMField);
begin
  Reset;
  FieldType := Field.FieldType;
  FieldName := Field.FieldName;
  TableName := Field.TableName;
end;

function TMRecordField.DateToFirebirdString(const Date: TDateTime): string;
var
  Year,
  Day,
  Month : word;
  YearStr,
  DayStr,
  MonthStr : string;
  Delimiter : string;
begin
  Delimiter := '.';

  DecodeDate(Date, Year, Month, Day);

  YearStr := IntToStr(Year);
  if Month < 10
    then MonthStr := '0' + IntToStr(Month)
    else MonthStr := IntToStr(Month);

  if Day < 10
    then DayStr := '0' + IntToStr(Day)
    else DayStr := IntToStr(Day);

  Result := YearStr + Delimiter + MonthStr + Delimiter + DayStr;
end;

function TMRecordField.DateToMySQLString(Date : TDateTime) : string;
var
  Year,
  Day,
  Month : word;
  YearStr,
  DayStr,
  MonthStr : string;
  Delimiter : string;
begin
  Delimiter := '-';

  DecodeDate(Date, Year, Month, Day);

  YearStr := IntToStr(Year);
  if Month < 10
    then MonthStr := '0' + IntToStr(Month)
    else MonthStr := IntToStr(Month);

  if Day < 10
    then DayStr := '0' + IntToStr(Day)
    else DayStr := IntToStr(Day);


  Result := YearStr + Delimiter + MonthStr + Delimiter + DayStr;
end;

function TMRecordField.TimeToFirebirdString(const Date: TDateTime): string;
var
  Hour,
  Minute,
  Second,
  MSeconds : word;
  HourStr,
  MinuteStr,
  SecondStr : string;
  Delimiter : string;
begin
  Delimiter := ':';

  DecodeTime(Date, Hour, Minute, Second, MSeconds);

  if Hour < 10
    then HourStr := '0' + IntToStr(Hour)
    else HourStr := IntToStr(Hour);

  if Minute < 10
    then MinuteStr := '0' + IntToStr(Minute)
    else MinuteStr := IntToStr(Minute);

  if Second < 10
    then SecondStr := '0' + IntToStr(Second)
    else SecondStr := IntToStr(Second);

  Result := HourStr + Delimiter + MinuteStr + Delimiter + SecondStr;
end;

function TMRecordField.TimeToMySQLString(Date : TDateTime) : string;
var
  Hour,
  Minute,
  Second,
  MSeconds : word;
  HourStr,
  MinuteStr,
  SecondStr : string;
  Delimiter : string;
begin
  Delimiter := ':';

  DecodeTime(Date, Hour, Minute, Second, MSeconds);


  if Hour < 10
    then HourStr := '0' + IntToStr(Hour)
    else HourStr := IntToStr(Hour);


  if Minute < 10
    then MinuteStr := '0' + IntToStr(Minute)
    else MinuteStr := IntToStr(Minute);

  if Second < 10
    then SecondStr := '0' + IntToStr(Second)
    else SecondStr := IntToStr(Second);


  Result := HourStr + Delimiter + MinuteStr + Delimiter + SecondStr;
end;


function TMRecordField.DateTimeToFirebirdString(const Date: TDateTime): string;
begin
  Result := DateToFirebirdString(Date) + ' ' +
            TimeToFirebirdString(Date);
end;

function TMRecordField.DateTimeToMySQLString(const Date : TDateTime) : string;
begin
  Result := DateToMySQLString(Date) + ' ' +
            TimeToMySQLString(Date);
end;

//////////////TMRecordFieldSSSSSS//////////////////////////////////
constructor TMRecordFields.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMRecordFields.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;


function TMRecordFields.GetItem(ItemIndex : integer) : TMRecordField;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMRecordField;
end;


function TMRecordFields.GetItem(ItemName : string) : TMRecordField;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemName);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMRecordFields.AddItem(Item : TMRecordField) : TMRecordField;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.FieldName, Item) >= 0
    then Result := Item;
end;

function TMRecordFields.AddItems(RecordFields : TMRecordFields) : integer;
var
  i,
  RecordFieldsCount : integer;
  RecordField : TMRecordField;
begin
  RecordFieldsCount := RecordFields.GetCount;
  Result := 0;

  for i := 0 to RecordFieldsCount - 1 do
  begin
    RecordField := RecordFields.GetItem(i);

    if not Assigned(RecordField)
      then Continue;

    if Assigned(AddItem(RecordField))
      then INC(Result);
  end;

end;


function TMRecordFields.DeleteItem(ItemName : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemName);

  Result := DeleteItem(ItemIndex);
end;


function TMRecordFields.DeleteItem(ItemIndex : integer) : boolean;
var
  RecordField : TMRecordField;
begin
  Result := FALSE;
  RecordField := GetItem(ItemIndex);
  if Assigned(RecordField) then
    begin
      Items.Delete(ItemIndex);
      RecordField.Free;
      Result := TRUE;
    end;
end;

procedure TMRecordFields.Reset;
var
  i, count : integer;
  RecordField : TMRecordField;
begin
  count := GetCount;
  for i := 0 to count - 1 do
  begin
    RecordField := Items.Objects[i] as TMRecordField;
    if Assigned(RecordField)
      then RecordField.Free;
  end;

  Items.Clear;
end;

function TMRecordFields.GetCount : integer;
begin
  Result := Items.Count;
end;

end.
