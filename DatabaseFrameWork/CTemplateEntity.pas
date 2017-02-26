unit CTemplateEntity;

interface

uses CTableRecords;

type
  TMTemplateEntity = class
    constructor Create();
    destructor Destroy(); override;

  protected
    FTableRecord : TMTableRecord;
    FTableName : string;

    procedure GetTableRecordData(); virtual; abstract;   //Установка значений полей класса наследника из TableRecord
    procedure SetTableRecordData(); virtual; abstract;   //Установка данных из полей класса наследника
    procedure SetPKFieldValue(Value : variant); virtual; abstract;

    function GenerateUid() : string;

    //procedure SetTableName(Name : string);

    procedure Reset(); virtual;

  public
    function SaveToDataBase() : integer;  virtual;
    function LoadFromDataBase(ID : integer) : integer; overload; virtual;
    function LoadFromDataBase(FieldName : string; FieldValue : variant): integer; overload; virtual;
    procedure DeleteFromDataBase(ID : integer);  overload; virtual;
    procedure DeleteFromDataBase(); overload; virtual;

  public
    property TableName : string read FTableName;

  end;

implementation

uses Variants;

constructor TMTemplateEntity.Create();
begin
  FTableRecord := TMTableRecord.Create(FTableName);

  Reset;
end;

destructor TMTemplateEntity.Destroy;
begin
  Reset;

  FTableRecord.Free;

  inherited;
end;

procedure TMTemplateEntity.Reset();
begin
  FTableRecord.ClearRecordValues;
  FTableRecord.QueryConstructor.ClearConditions;
end;

function TMTemplateEntity.GenerateUid() : string;
var
  i : integer;
  randomIndex : integer;
const
  Uid_Length = 20;
  Uid_Alphabet = '0123456789abcdefgh';
begin
  Result := '';
  randomize;
  for i := 0 to Uid_Length - 1 do
    begin
      RandomIndex := random(Length(Uid_Alphabet));
      result := result + Uid_Alphabet[RandomIndex + 1];
    end;
end;

function TMTemplateEntity.SaveToDataBase() : integer;
var
  PKFieldValue : variant;
  PKFieldName : string;
begin
  Result := 0;

  if not Assigned(FTableRecord)
    then Exit;

  SetTableRecordData;  //Устанавливаем значение для FTableRecord

  PKFieldName := FTableRecord.PKFieldName;

  if PKFieldName = ''
    then Exit;

  PKFieldValue := FTableRecord.FieldByName[PKFieldName].Value;

  if (PKFieldValue = null) or (PKFieldValue = Unassigned)
    then Result := FTableRecord.AddRecord
    else Result := FTableRecord.SaveRecord;

  SetPKFieldValue(Result);
end;


function TMTemplateEntity.LoadFromDataBase(ID : integer) : integer;
begin
  Reset;

  Result := FTableRecord.LoadRecord(ID);

  if Result > 0
    then GetTableRecordData;
end;

function TMTemplateEntity.LoadFromDataBase(FieldName : string; FieldValue : variant): integer;
begin
  Reset;

  Result := FTableRecord.LoadRecord(FieldName, FieldValue);

  if Result > 0
    then GetTableRecordData;  
end;

procedure TMTemplateEntity.DeleteFromDataBase(ID : integer);
begin
  FTableRecord.DeleteRecord(ID);
end;

procedure TMTemplateEntity.DeleteFromDataBase();
begin
  FTableRecord.DeleteRecord;
end;

end.
