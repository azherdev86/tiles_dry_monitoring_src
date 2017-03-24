{**************************************************************************
 ласс TMTables €вл€етс€ полем TMDataBaseStructure
¬ этих таблицах не содержатс€ данные, а только информаци€ о структуре данных:
- список полей с указанием типов данных;
- указание пол€ первичного ключа;
- указание пол€ внешнего ключа;

примечание:
ѕервичные ключи не фигурируют в списке полей TMTable
¬ классе CTableRecords, объекты которого создаютс€ методом
Create(TableName) на основании структуры таблицы ключи
добавл€ютс€ с типом данных Variant.
}

unit CTableStructure;

interface

uses
  Classes, DBTables, CFields, DB;


type
  TMTableStructure = class
    constructor Create;
    destructor Destroy; override;

  public
    TableName : string;
    PKFieldName : string;
    Fields : TMFields;

    procedure Reset;
    function AddField(FieldName : string; FieldType : TFieldType) : boolean;
    function AddForeignKey(FieldName : string; FieldType : TFieldType; ForeignTableName : string)  : boolean;
    function GetForeignKeyFieldName(ForeignTableName : string) : string;
    function GetForeignKey(ForeignTableName : string) : TMForeignKey;
    function GetPKField : TMField;

    function GetField(FieldName : string) : TMField;
end;

type
  TMTables = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMTableStructure; overload;
    function GetItem(ItemName : string) : TMTableStructure; overload;

    function AddItem(Item : TMTableStructure) : TMTableStructure;
    function DeleteItem(ItemName : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;

  end;
implementation


constructor TMTableStructure.Create;
begin
  Fields := TMFields.Create;
  Reset;
end;


destructor TMTableStructure.Destroy;
begin
  Reset;
  Fields.Free;
  inherited Destroy;  
end;

procedure TMTableStructure.Reset;
begin
  Fields.Reset;
  TableName := '';
  PKFieldName := '';
end;

function TMTableStructure.AddField(FieldName : string; FieldType : TFieldType) : boolean;
var
  Field : TMField;
begin
  Result := False;
  Field := Fields.GetItem(FieldName);
  //≈сли поле с таким именем уже существует, то завершаем работу
  if Assigned(Field)
    then Exit;

  Field := TMField.Create;
  Field.FieldName := FieldName;
  Field.FieldType := FieldType;
  Field.TableName := TableName;

  Field := Fields.AddItem(Field);

  if not Assigned(Field)
    then
      begin
        Field.Free;
        Exit
      end
    else Result := TRUE; 
end;


function TMTableStructure.AddForeignKey(FieldName : string; FieldType : TFieldType; ForeignTableName : string) : boolean;
var
  ForeignKeyField : TMForeignKey;
begin
  Result := False;
  ForeignKeyField := Fields.GetItem(FieldName) as TMForeignKey;
  //≈сли поле с таким именем уже существует, то завершаем работу
  if Assigned(ForeignKeyField)
    then Exit;

  ForeignKeyField := TMForeignKey.Create;
  ForeignKeyField.FieldName := FieldName;
  ForeignKeyField.FieldType := FieldType;
  ForeignKeyField.ForeignTableName := ForeignTableName;
  ForeignKeyField.TableName := TableName;

  ForeignKeyField := Fields.AddItem(ForeignKeyField) as TMForeignKey;

  if not Assigned(ForeignKeyField)
    then
      begin
        ForeignKeyField.Free;
        Exit
      end
    else Result := TRUE;
end;

function TMTableStructure.GetField(FieldName : string) : TMField;
begin
  Result := Fields.GetItem(FieldName);
end;

function TMTableStructure.GetPKField : TMField;
begin
  Result := GetField(PKFieldName);
end;


function TMTableStructure.GetForeignKeyFieldName(ForeignTableName : string) : string;
var
  ForeignKey : TMForeignKey;
begin
  Result := '';

  ForeignKey := GetForeignKey(ForeignTableName);
  if Assigned(ForeignKey)
    then Result := ForeignKey.FieldName;
end;

function TMTableStructure.GetForeignKey(ForeignTableName : string) : TMForeignKey;
var
  ForeignKey : TMForeignKey;
  i : integer;
  FieldsCount : integer;
begin
  Result := nil;

  FieldsCount := Fields.GetCount;

  for i := 0 to FieldsCount - 1 do
  begin
    if Fields.GetItem(i) is TMForeignKey
      then
        begin
          ForeignKey := Fields.GetItem(i) as TMForeignKey;
          if ForeignKey.ForeignTableName = ForeignTableName
            then
              begin
                Result := ForeignKey;
                break;
              end
//            else
//              ForeignKey := nil;
        end;
  end;                          
end;


//////////////TMTableSSSSSS//////////////////////////////////
constructor TMTables.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMTables.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;


function TMTables.GetItem(ItemIndex : integer) : TMTableStructure;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMTableStructure;
end;


function TMTables.GetItem(ItemName : string) : TMTableStructure;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemName);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMTables.AddItem(Item : TMTableStructure) : TMTableStructure;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.TableName, Item) >= 0
    then Result := Item;
end;

function TMTables.DeleteItem(ItemName : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemName);

  Result := DeleteItem(ItemIndex);
end;


function TMTables.DeleteItem(ItemIndex : integer) : boolean;
var
  Table : TMTableStructure;
begin
  Result := FALSE;
  Table := GetItem(ItemIndex);
  if Assigned(Table) then
    begin
      Items.Delete(ItemIndex);
      Table.Free;
      Result := TRUE;
    end;
end;

procedure TMTables.Reset;
var
  i, count : integer;
  Table : TMTableStructure;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    Table := Items.Objects[i] as TMTableStructure;
    if Assigned(Table)
      then Table.Free;
  end;

  Items.Clear;
end;

function TMTables.GetCount : integer;
begin
  Result := Items.Count;
end;


end.
