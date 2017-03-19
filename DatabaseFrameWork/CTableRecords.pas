unit CTableRecords;

interface

uses //DBTables,
  ZDataset,
  Classes, CRecordFields, CQueryConstructor, CDataBaseStructure, CTableStructure;

type
  TMTableRecords = class; //forward declaration
  TMTableRecord = class
    constructor Create(tblName : string); overload;
    constructor Create(); overload;
    destructor Destroy; override;

  public
    procedure Reset;
    procedure ClearRecordValues;
  public
    //основаны на методах LoadRecord - для одной записи - возвращает ID
    //LoadRecords - для нескольких записей - возвращает количество полученных записей
    //***********************************************//
    function LoadRecords : integer; overload; //возвращается количество записей
    function LoadRecord : integer; overload; //возвращается ID первой загруженной записи
    //***********************************************//
    function LoadRecordsAll : integer; //загрузка всех записей таблицы
    function LoadRecord(RecordID : integer) : integer; overload; //загрузка записи с указанным ID - возвращае RecordID
    function LoadRecord(FieldName : String; FieldValue : variant) : integer; overload;
    function LoadRecords(FieldName : String; FieldValue : variant) : integer; overload;
    //загрузка записей с 2-мя и 3-мя условиями. Operation AND
    function LoadRecordsAND(FieldName1 : String; FieldValue1 : variant;
                            FieldName2 : String; FieldValue2 : variant) : integer; overload;
    function LoadRecordsAND(FieldName1 : String; FieldValue1 : variant;
                            FieldName2 : String; FieldValue2 : variant;
                            FieldName3 : String; FieldValue3 : variant) : integer; overload;
    //загрузка записей с 2-мя и 3-мя условиями. Operation OR
    function LoadRecordsOR(FieldName1 : String; FieldValue1 : variant;
                           FieldName2 : String; FieldValue2 : variant) : integer; overload;
    function LoadRecordsOR(FieldName1 : String; FieldValue1 : variant;
                           FieldName2 : String; FieldValue2 : variant;
                           FieldName3 : String; FieldValue3 : variant) : integer; overload;
    //******************************************************//
    //Используется для сохранения единственной записи, у которой уже задан ID
    //Представляет собой обычный запрос UPDATE ... WHERE ID = ID_VALUE;
    function SaveRecord : integer; //возвращается ID записи
    //******************************************************//
    //Используется для задания значения каждому FieldName значения
    //FieldValue. В качестве условий используется QueryConstructor
    //Базовый метод для всех UpdateRecords - private UpdateRecords(UpdatingFields : TMRecordFields
    //!!!!!!!!!Все методы возвращают LoadRecords с условиями QueryConstuctor, по которым
    //потом можно осуществлять навигацию SetCurrentRecordIndex();
    function UpdateRecords(ConditionFieldName : string; ConditionFieldValue : variant;
                           UpdatingFieldName : string; UpdatingFieldValue : variant) : integer; overload;

    function UpdateRecord(PKFieldValue : variant; UpdatingFieldName : string;
                          UpdatingFieldValue : variant) : integer; overload;

    function UpdateRecords(FieldName : string; FieldValue : variant) : integer; overload;

    //Обновляются все записи таблицы без проверки условий
    function UpdateRecordsAll(FieldName : string; FieldValue : variant) : integer; overload;
    function UpdateRecordsAll(FieldName1 : string; FieldValue1 : variant;
                              FieldName2 : string; FieldValue2 : variant) : integer; overload;
    //******************************************************//
    //Добавление новой записи. Все первичные ключи AUTO_INCREMENT
    //поэтому ID в запросе INSERT не участвует
    function AddRecord : integer; //с возвратом ID
    procedure AddRecordWithoutReturn; //без возврата ID (быстрее работает)

    //******************************************************//
    //Удаление записей из таблицы
    procedure DeleteRecord; overload; //удаление текущей записи
    procedure DeleteRecord(RecordID : integer); overload; //удаление записи с заданным ID
    procedure DeleteRecordsAll; //удаление всех записей в таблице
    procedure DeleteRecords; overload;
    procedure DeleteRecords(FieldName : string; FieldValue : variant); overload;
    procedure DeleteRecords(FieldName1 : string; FieldValue1 : variant;
                            FieldName2 : string; FieldValue2 : variant); overload;

    function DeleteRecordsRowsAffected : integer;

    //******************************************************************
    //***********АГРЕГАТНЫЕ ФУНКЦИИ*************************************
    //******************************************************************
    function GetMaxValue(FieldName : string) : integer;

    function SetCurrentRecordIndex(RecordIndex : integer) : boolean;
    function GetFieldValues : string;

    function LoadTableStructure(tblName : string) : boolean;

  private

    //Используется в качестве базовой для других функций UpdateRecords и UpdateRecordsAll
    function UpdateRecords(UpdatingFields : TMRecordFields) : integer; overload;
    procedure AddRecordMySQL; //без возврата ID
    function AddRecordFireBird : integer; //с возвратом ID

    function GetFieldByName(FieldName : string) : TMRecordField;
    function GetFieldByIndex(FieldIndex : integer) : TMRecordField;
    function GetPKFieldValue() : variant;

    function AddField(Field : TMRecordField) : TMRecordField;
    function DeleteField(FieldName : string) : boolean; overload;
    function DeleteField(FieldIndex : integer) : boolean; overload;

    function GetRecordValues : boolean;

    //***************************************************************
    //Получение текста запроса для методов LOAD, SAVE, UPDATE, INSERT
    function GetSelectQueryString() : string; //для методов Load
    function GetSaveQueryString() : string; //для метода Save
    //UpdatingFields - список полей, с указанными значениями, которые нужно обновить
    function GetUpdateQueryString(UpdatingFields : TMRecordFields) : string; //для методов Update
    function GetInsertQueryString() : string; //для метода Add
    function GetDeleteQueryString() : string; //для метода Delete
    function GetMaxQueryString(FieldName : string) : string;

    function GetFieldList(ShowPK : boolean = false) : string;
    //чтобы за один прхоход цикла получить и поля и значения используем процедуру с параметрами VAR
    procedure GetFieldValueStrings(var FieldClause, ValueClause : string); //для метода GetInsertQueryString
    //вовзращает строку Field[i] = value[i], ...,
    //для метода SaveRecord
    function GetField_ValueList(ShowPK : boolean = false) : string; overload;
    //для метода UpdateRecord Field_valuesList должен возвращать только те поля, которые нужно обновлять
    function GetField_ValueList(UpdatingFields : TMRecordFields; ShowPK : boolean = false) : string; overload;
    function GetFieldsCount : integer;

  private
    FQuery : TZQuery;
    Items : TStringList;
    FPKFieldName : string;
    FTableName : string;
    FCurrentRecordIndex : integer;
    FRecordsCount : integer;
    FDataBaseType : TMDatabaseType;
    //По умолчанию используется FDefaultQueryConstructor
    //второй добавлен для обеспечения совместимости с TableBrowse
    //чтобы можно было легко получить доступ к условиям TableBrowse
    FQueryConstructor : TMQueryConstructor;
    FTableStructureRef : TMTableStructure;

  public
    property FieldByName[FieldName : string] : TMRecordField read GetFieldByName;
    property FieldByIndex[FieldIndex : integer] : TMRecordField read GetFieldByIndex;
    property FieldsCount : integer read GetFieldsCount;

    property TableName : string read FTableName;
    property PKFieldName : string read FPKFieldName;
    property PKFieldValue : variant read GetPKFieldValue;
    property RecordsCount : integer read FRecordsCount;
    property QueryConstructor : TMQueryConstructor read FQueryConstructor write FQueryConstructor;
 end;

  TMTableRecords = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMTableRecord; overload;
    function GetItem(ItemTitle : string) : TMTableRecord; overload;

    function AddItem(Item : TMTableRecord) : TMTableRecord;
    function DeleteItem(ItemTitle : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;
  end;

implementation

uses LApplicationGlobals,
     DB,
     CFields,
     Dialogs,
     Variants,
     SysUtils,
     CConditions;


constructor TMTableRecord.Create(tblName : string);
begin
  Self.Create();
  LoadTableStructure(tblName);

  //В деструкторе описывать не нужно
  FTableStructureRef := ApplicationDataBaseStructure.GetTable(FTableName);
end;

constructor TMTableRecord.Create();
begin
  FQuery := TZQuery.Create(ApplicationDBConnection);
  FQuery.Connection := ApplicationDBConnection;
  Items := TStringList.Create;
  FQueryConstructor := TMQueryConstructor.Create;
  Reset;
end;


destructor TMTableRecord.Destroy;
begin
  Reset;

  FQuery.Active := FALSE;
  FQuery.Free;
  FQueryConstructor.Free;
  Items.Free;
  inherited;
end;

function TMTableRecord.LoadRecordsAll : integer;
begin
  FQueryConstructor.ClearConditions;
  Result := LoadRecords;
end;

function TMTableRecord.LoadRecord(RecordID : integer) : integer;
var
  RecordField : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField := FieldByName[PKFieldName];
  //Если поля не существует
  if not Assigned(RecordField)
    then Exit;
    
  QueryConstructor.AddCondition(RecordField, ctEqual, RecordID);
  Result := LoadRecord;
end;

function TMTableRecord.LoadRecord(FieldName : String; FieldValue : variant) : integer;
var
  RecordField : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField := FieldByName[FieldName];
  //Если поля не существует
  if not Assigned(RecordField)
    then Exit;

  QueryConstructor.AddCondition(RecordField, ctEqual, FieldValue);
  Result := LoadRecord;
end;

function TMTableRecord.LoadRecords(FieldName : String; FieldValue : variant) : integer;
var
  RecordField : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField := FieldByName[FieldName];
  //Если поля не существует
  if not Assigned(RecordField)
    then Exit;

  QueryConstructor.AddCondition(RecordField, ctEqual, FieldValue);
  Result := LoadRecords;
end;

//загрузка записей с 2-мя и 3-мя условиями. Operation AND
function TMTableRecord.LoadRecordsAND(FieldName1 : String; FieldValue1 : variant;
                                       FieldName2 : String; FieldValue2 : variant) : integer;
var
  RecordField1,
  RecordField2 : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField1 := FieldByName[FieldName1];
  //Если поля не существует
  if not Assigned(RecordField1)
    then Exit;

  RecordField2 := FieldByName[FieldName2];
  //Если поля не существует
  if not Assigned(RecordField2)
    then Exit;

  QueryConstructor.Operation := otAND;
  QueryConstructor.AddCondition(RecordField1, ctEqual, FieldValue1);
  QueryConstructor.AddCondition(RecordField2, ctEqual, FieldValue2);

  Result := LoadRecords;

end;

function TMTableRecord.LoadRecordsAND(FieldName1 : String; FieldValue1 : variant;
                                       FieldName2 : String; FieldValue2 : variant;
                                       FieldName3 : String; FieldValue3 : variant) : integer;
var
  RecordField1,
  RecordField2,
  RecordField3 : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField1 := FieldByName[FieldName1];
  //Если поля не существует
  if not Assigned(RecordField1)
    then Exit;

  RecordField2 := FieldByName[FieldName2];
  //Если поля не существует
  if not Assigned(RecordField2)
    then Exit;

  RecordField3 := FieldByName[FieldName3];
  //Если поля не существует
  if not Assigned(RecordField3)
    then Exit;

  QueryConstructor.Operation := otAND;
  QueryConstructor.AddCondition(RecordField1, ctEqual, FieldValue1);
  QueryConstructor.AddCondition(RecordField2, ctEqual, FieldValue2);
  QueryConstructor.AddCondition(RecordField3, ctEqual, FieldValue3);

  Result := LoadRecords;
end;

//загрузка записей с 2-мя и 3-мя условиями. Operation OR
function TMTableRecord.LoadRecordsOR(FieldName1 : String; FieldValue1 : variant;
                                      FieldName2 : String; FieldValue2 : variant) : integer;
var
  RecordField1,
  RecordField2 : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField1 := FieldByName[FieldName1];
  //Если поля не существует
  if not Assigned(RecordField1)
    then Exit;

  RecordField2 := FieldByName[FieldName2];
  //Если поля не существует
  if not Assigned(RecordField2)
    then Exit;

  QueryConstructor.Operation := otOR;

  QueryConstructor.AddCondition(RecordField1, ctEqual, FieldValue1);
  QueryConstructor.AddCondition(RecordField2, ctEqual, FieldValue2);


  Result := LoadRecords;

end;

function TMTableRecord.LoadRecordsOR(FieldName1 : String; FieldValue1 : variant;
                                      FieldName2 : String; FieldValue2 : variant;
                                      FieldName3 : String; FieldValue3 : variant) : integer;
var
  RecordField1,
  RecordField2,
  RecordField3 : TMRecordField;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  RecordField1 := FieldByName[FieldName1];
  //Если поля не существует
  if not Assigned(RecordField1)
    then Exit;

  RecordField2 := FieldByName[FieldName2];
  //Если поля не существует
  if not Assigned(RecordField2)
    then Exit;

  RecordField3 := FieldByName[FieldName3];
  //Если поля не существует
  if not Assigned(RecordField3)
    then Exit;

  QueryConstructor.Operation := otOR;
  QueryConstructor.AddCondition(RecordField1, ctEqual, FieldValue1);
  QueryConstructor.AddCondition(RecordField2, ctEqual, FieldValue2);
  QueryConstructor.AddCondition(RecordField3, ctEqual, FieldValue3);

  Result := LoadRecords;
end;

procedure TMTableRecord.AddRecordMySQL;
var
  SQLQuery : String;
begin
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;

  SQLQuery := GetInsertQueryString;
  FQuery.SQL.Text := SQLQuery;

  FQuery.ExecSQL;
end;

function TMTableRecord.AddRecordFireBird : integer;
var
  SQLQuery : String;
begin
  Result := 0;

  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;

  SQLQuery := GetInsertQueryString;
  FQuery.SQL.Text := SQLQuery;

  FQuery.Active := TRUE;

  while not FQuery.Eof do
    begin
      Result := FQuery.FieldByName(FPKFieldName).AsInteger;
      FQuery.Next;
    end;
end;

function TMTableRecord.AddRecord() : integer;
begin
  Result := 0;
  FQueryConstructor.ClearConditions;

  case FDataBaseType of
    dtMySQL:
      begin
        AddRecordMySQL;
        FQuery.Active := FALSE;
        FQuery.SQL.Text := 'SELECT LAST_INSERT_ID() as "ID"';
        FQuery.Active := TRUE;

        while not FQuery.Eof do
        begin
          Result := FQuery.FieldByName('ID').AsInteger;
          FQuery.Next;
        end;
      end;
    dtFireBird:
      begin
        Result := AddRecordFireBird;   
      end;
  end;
end;

procedure TMTableRecord.AddRecordWithoutReturn;
begin
  case FDataBaseType of
    dtMySQL: AddRecordMySQL;
    dtFireBird: ;
  end;
end;


function TMTableRecord.SetCurrentRecordIndex(RecordIndex : integer) : boolean;
begin
  Result := false;

  if RecordIndex < 0
    then Exit;

  if FCurrentRecordIndex = RecordIndex
    then
      begin
        Result := TRUE;
        Exit;
      end;

  if not FQuery.Active
    then Exit;

  FQuery.MoveBy(RecordIndex - FCurrentRecordIndex);

  ClearRecordValues;
  GetRecordValues;

  FCurrentRecordIndex := RecordIndex;

  Result := (FCurrentRecordIndex >= 0);
end;

function TMTableRecord.GetFieldValues : string;
var
  i : integer;
  RecordField : TMRecordField;
begin
  Result := '';
  for i := 0 to GetFieldsCount - 1 do
  begin
    RecordField := GetFieldByIndex(i);

    if not Assigned(RecordField)
      then continue;


    Result := Result + RecordField.FieldName + ' = ' +
              RecordField.AsString + sLineBreak;
  end;


end;

function TMTableRecord.GetFieldByName(FieldName : string) : TMRecordField;
var
  FieldIndex : integer;
begin
  Result := nil;

  FieldIndex := Items.IndexOf(FieldName);

  if FieldIndex >= 0
    then Result := GetFieldByIndex(FieldIndex);
end;

function TMTableRecord.GetFieldByIndex(FieldIndex : integer) : TMRecordField;
begin
  Result := nil;
  if FieldIndex < 0
    then Exit;

  Result := Items.Objects[FieldIndex] as TMRecordField;
end;


function TMTableRecord.GetPKFieldValue() : variant;
begin
  Result := GetFieldByName(FPKFieldName).Value;
end;


function TMTableRecord.AddField(Field : TMRecordField) : TMRecordField;
begin
  Result := nil;

  if not Assigned(Field)
    then Exit;

  if Items.AddObject(Field.FieldName, Field) >= 0
    then Result := Field;
end;

function TMTableRecord.DeleteField(FieldName : string) : boolean;
var
  FieldIndex : integer;
begin
  FieldIndex := Items.IndexOf(FieldName);

  Result := DeleteField(FieldIndex);
end;

function TMTableRecord.DeleteField(FieldIndex : integer) : boolean;
var
  Field : TMRecordField;
begin
  Result := FALSE;
  Field := GetFieldByIndex(FieldIndex);
  if Assigned(Field) then
    begin
      Items.Delete(FieldIndex);
      Field.Free;
      Result := TRUE;
    end;
end;

procedure TMTableRecord.Reset;
var
  i : integer;
  Field : TMRecordField;
begin
  for i := 0 to GetFieldsCount - 1 do
  begin
    Field := Items.Objects[i] as TMRecordField;
    if Assigned(Field)
      then Field.Free;
  end;

  Items.Clear;
  FQueryConstructor.ClearConditions;

  FPKFieldName := '';
  FTableName := '';
  FQuery.Active := False;
  FQuery.SQL.Text := '';
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;
end;

procedure TMTableRecord.ClearRecordValues;
var
  i : integer;
begin
  for i := 0 to GetFieldsCount - 1 do
    FieldByIndex[i].Value := null;
end;

function TMTableRecord.GetFieldsCount : integer;
begin
  Result := Items.Count;
end;


function TMTableRecord.LoadTableStructure(tblName : string) : boolean;
var
  Table : TMTableStructure;
  i : integer;
  RecordField : TMRecordField;
  Field : TMField;
begin
  Result := False;

  FDataBaseType := ApplicationDataBaseStructure.DatabaseType;

  Table := ApplicationDataBaseStructure.Tables.GetItem(tblName);
  if not Assigned(Table)
    then Exit;

  FTableName := Table.TableName;
  FPKFieldName := Table.PKFieldName;

  //загружаем информацию обо всех полях таблицы
  for i := 0 to Table.Fields.GetCount - 1 do
  begin
    Field := Table.Fields.GetItem(i);
    if not Assigned(Field)
      then Continue;

    RecordField := TMRecordField.Create(Field);
    if not Assigned(AddField(RecordField))
      then RecordField.Free;
  end;
end;


function TMTableRecord.GetSelectQueryString() : string;
var
  WhereClause,
  FieldsClause : string;
begin
  FieldsClause := GetFieldList(True);
  WhereClause := FQueryConstructor.GetWhereClause;
  Result := 'SELECT ' + FieldsClause + 'FROM ' + TableName + ' ' +
            WhereClause;
end;

function TMTableRecord.GetMaxQueryString(FieldName : string) : string;
var
  WhereClause : string;
begin
  WhereClause := FQueryConstructor.GetWhereClause();
  Result := 'SELECT MAX(' + FieldName + ') as "max" FROM ' + TableName + ' ' +
            WhereClause;
end;

function TMTableRecord.GetSaveQueryString() : string;
var
  WhereClause,
  FieldsClause : string;
  KeyField : TMRecordField;
begin
  Result := '';

  FieldsClause := GetField_ValueList();

  KeyField := GetFieldByName(FPKFieldName);
  if not Assigned(KeyField)
    then Exit;

  WhereClause := FTableName + '.' + KeyField.FieldName +
                 '=' + KeyField.ValueString;

  Result := 'UPDATE ' + FTableName + ' SET ' +
            FieldsClause + ' WHERE ' + WhereClause;
end;


function TMTableRecord.GetUpdateQueryString(UpdatingFields : TMRecordFields) : string; //для методов Update
var
  WhereClause,
  FieldsClause : string;
begin
  Result := '';

  FieldsClause := GetField_ValueList(UpdatingFields);

  WhereClause := FQueryConstructor.GetWhereClause();
  
  Result := 'UPDATE ' + FTableName + ' SET ' +
            FieldsClause + WhereClause;

end;

function TMTableRecord.GetInsertQueryString() : string; //для метода Add
var
  FieldsClause,
  ValuesClause : string;
begin
  Result := '';

  GetFieldValueStrings(FieldsClause, ValuesClause);

  Result := 'INSERT INTO ' + FTableName + ' (' +
            FieldsClause + ') VALUES (' + ValuesClause + ')';

  if FDataBaseType = dtFireBird
    then Result := Result + ' RETURNING ' + FPKFieldName;
  
end;

function TMTableRecord.GetDeleteQueryString() : string; //для метода Add
var
  WhereClause : string;
begin
  WhereClause := FQueryConstructor.GetWhereClause();
  Result := 'DELETE FROM ' + TableName + ' ' +
            WhereClause;
end;

function TMTableRecord.GetFieldList(ShowPK : boolean = false) : string;
var
  i : integer;
  RecordField : TMRecordField;
  FieldsCount : integer;
  ItemIsAdded : boolean;
begin
  Result := '';

  FieldsCount := GetFieldsCount;
  for i := 0 to FieldsCount - 1 do
  begin
    ItemIsAdded := FALSE;
    RecordField := FieldByIndex[i];
    if not Assigned(RecordField)
      then Continue;

    if RecordField.FieldName = FPKFieldName
      then
        begin
          if ShowPK
            then
              begin
                Result := Result + FTableName + '.' + RecordField.FieldName;
                ItemIsAdded := TRUE;
              end;
        end
      else
        begin
          Result := Result + FTableName + '.' + RecordField.FieldName;
          ItemIsAdded := TRUE;
        end;

    if ((i <> (FieldsCount - 1)) and ItemIsAdded)
      then Result := Result + ', '
      else Result := Result + ' ';
  end;
end;

procedure TMTableRecord.GetFieldValueStrings(var FieldClause, ValueClause : string);
var
  i : integer;
  RecordField : TMRecordField;
  FieldsCount : integer;
begin
  FieldClause := '';
  ValueClause := '';

  FieldsCount := GetFieldsCount;
  for i := 0 to FieldsCount - 1 do
  begin
    RecordField := FieldByIndex[i];
    if not Assigned(RecordField)
      then Continue;

    if RecordField.FieldName = FPKFieldName
      then Continue;

    FieldClause := FieldClause + FTableName + '.' + RecordField.FieldName;
    ValueClause := ValueClause + RecordField.ValueString;


    if (i <> (FieldsCount - 1))
      then
        begin
          FieldClause := FieldClause + ', ';
          ValueClause := ValueClause + ', ';

        end
      else
        begin
          FieldClause := FieldClause + ' ';
          ValueClause := ValueClause + ' ';
        end;
  end;
end;
function TMTableRecord.GetField_ValueList(ShowPK : boolean = false) : string;
var
  i : integer;
  RecordField : TMRecordField;
  FieldsCount : integer;
  ItemIsAdded : boolean;
begin
  Result := '';

  FieldsCount := GetFieldsCount;
  for i := 0 to FieldsCount - 1 do
  begin
    ItemIsAdded := False;
    RecordField := FieldByIndex[i];
    if not Assigned(RecordField)
      then Continue;

    if RecordField.FieldName = FPKFieldName
      then
        begin
          if ShowPK
            then
              begin
                Result := Result + FTableName + '.' + RecordField.FieldName +
                          '=' + RecordField.ValueString;
                ItemIsAdded := TRUE;
              end;
        end
      else
        begin
          Result := Result + FTableName + '.' + RecordField.FieldName +
                    '=' + RecordField.ValueString;
          ItemIsAdded := TRUE;
        end;


    if (i <> (FieldsCount - 1)) and (ItemIsAdded)
      then Result := Result + ', '
      else Result := Result + ' ';
  end;
end;

function TMTableRecord.GetField_ValueList(UpdatingFields : TMRecordFields; ShowPK : boolean = false) : string;
var
  i : integer;
  RecordField : TMRecordField;
  FieldsCount : integer;
  ItemIsAdded : boolean;
begin
  Result := '';

  FieldsCount := UpdatingFields.GetCount;
  for i := 0 to FieldsCount - 1 do
  begin
    ItemIsAdded := False;
    RecordField := UpdatingFields.GetItem(i);
    if not Assigned(RecordField)
      then Continue;

    if RecordField.FieldName = FPKFieldName
      then
        begin
          if ShowPK
            then
              begin
                Result := Result + FTableName + '.' + RecordField.FieldName +
                          '=' + RecordField.ValueString;
                ItemIsAdded := TRUE;
              end;
        end
      else
        begin
          Result := Result + FTableName + '.' + RecordField.FieldName +
                    '=' + RecordField.ValueString;
          ItemIsAdded := TRUE;
        end;


    if (i <> (FieldsCount - 1)) and (ItemIsAdded)
      then Result := Result + ', '
      else Result := Result + ' ';
  end;
end;

function TMTableRecord.GetRecordValues : boolean;
var
  i : integer;
  FieldsCount : integer;
  FieldName : string;
begin
  Result := False;

  if not (FQuery.Active)
    then Exit;

  FieldsCount := FQuery.FieldCount;

  if FieldsCount = 0
    then Exit;

  for i := 0 to FieldsCount - 1 do
    begin
      FieldName := FQuery.Fields[i].FieldName;
      FieldByName[FieldName].Value := FQuery.Fields[i].Value;
    end;

  Result := TRUE;
end;



function TMTableRecord.LoadRecords : integer;
var
  SQLQuery : String;
begin
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;
  ClearRecordValues;

  SQLQuery := GetSelectQueryString;
  FQuery.SQL.Text := SQLQuery;

  FQuery.Active := TRUE;

  FRecordsCount := FQuery.RecordCount;

  Result := FRecordsCount;
  if Result > 0
    then GetRecordValues;

  FQueryConstructor.ClearConditions;
end;


function TMTableRecord.LoadRecord : integer;
var
  SQLQuery : String;
  tmpVariant : variant;
begin
  Result := 0;
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;
  ClearRecordValues;

  SQLQuery := GetSelectQueryString;
  FQuery.SQL.Text := SQLQuery;

  FQuery.Active := TRUE;


  if FQuery.RecordCount > 0
    then GetRecordValues;

  tmpVariant := FieldByName[PKFieldName].AsVariant;
  if tmpVariant <> null
    then
      begin
        FRecordsCount := 1;
        Result := tmpVariant;
      end;

  FQueryConstructor.ClearConditions;
end;

function TMTableRecord.SaveRecord : integer; //возвращается ID записи
var
  SQLQuery : String;
begin
  Result := 0;
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;
  FQueryConstructor.ClearConditions;

  SQLQuery := GetSaveQueryString;
  if SQLQuery = ''
    then Exit;

  FQuery.SQL.Text := SQLQuery;

  FQuery.ExecSQL;

  Result := GetFieldByName(FPKFieldName).AsInteger;
end;

function TMTableRecord.UpdateRecords(FieldName : string; FieldValue : variant) : integer;
var
  UpdatingFields : TMRecordFields;
  RecordField : TMRecordField;
begin
  Result := 0;

  RecordField := GetFieldByName(FieldName);

  //Если такого поля (FieldName) не существует
  if not Assigned(RecordField)
    then Exit;
  
  UpdatingFields := TMRecordFields.Create;
  try
    RecordField := UpdatingFields.AddItem(RecordField.CopyToRecordField);
    if not Assigned(RecordField)
      then Exit;

    RecordField.Value := FieldValue;
    
    Result := UpdateRecords(UpdatingFields);
  finally
    UpdatingFields.Free;
  end;
end;


function TMTableRecord.UpdateRecords(ConditionFieldName : string; ConditionFieldValue : variant;
                                      UpdatingFieldName : string; UpdatingFieldValue : variant) : integer;
var
  RecordField : TMRecordField;
begin
  RecordField := GetFieldByName(ConditionFieldName);
  FQueryConstructor.ClearConditions;
  QueryConstructor.AddCondition(RecordField, ctEqual, ConditionFieldValue);

  Result := UpdateRecords(UpdatingFieldName, UpdatingFieldValue);
end;

function TMTableRecord.UpdateRecord(PKFieldValue : variant; UpdatingFieldName : string;
                                      UpdatingFieldValue : variant) : integer;
begin
  Result := UpdateRecords(FPKFieldName, PKFieldValue, UpdatingFieldName, UpdatingFieldValue);
end;


//Обновляются все записи таблицы без проверки условий
function TMTableRecord.UpdateRecordsAll(FieldName : string; FieldValue : variant) : integer;
begin
  FQueryConstructor.ClearConditions;
  Result := UpdateRecords(FieldName, FieldValue);
end;

//Обновляются все записи таблицы без проверки условий
function TMTableRecord.UpdateRecordsAll(FieldName1 : string; FieldValue1 : variant;
                                         FieldName2 : string; FieldValue2 : variant) : integer;
begin
  FQueryConstructor.ClearConditions;
  Result := UpdateRecords(FieldName1, FieldValue1, FieldName2, FieldValue2);    
end;

//базовый приватный метод для всех UpdateRecords и UpdateRecordsALL
function TMTableRecord.UpdateRecords(UpdatingFields : TMRecordFields) : integer;
var
  SQLQuery : String;
begin
  Result := 0;

  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;

  SQLQuery := GetUpdateQueryString(UpdatingFields);
  if SQLQuery = ''
    then Exit;

  FQuery.SQL.Text := SQLQuery;
  FQuery.ExecSQL;

  Result := LoadRecords;
  //очищаем условия конструктора
  FQueryConstructor.ClearConditions;
end;

procedure TMTableRecord.DeleteRecords;
var
  SQLQuery : String;
begin
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;

  SQLQuery := GetDeleteQueryString;

  FQuery.SQL.Text := SQLQuery;
  FQuery.ExecSQL;

  //очищаем условия конструктора
  FQueryConstructor.ClearConditions;
end;

function TMTableRecord.DeleteRecordsRowsAffected : integer;
begin
  DeleteRecords;

  Result := FQuery.RowsAffected;
end;

procedure TMTableRecord.DeleteRecordsAll;
begin
  //Очищаем все условия в QueryConstructore
  FQueryConstructor.ClearConditions;
  DeleteRecords();
end;

procedure TMTableRecord.DeleteRecord; //удаление текущей записи
var
  KeyField : TMRecordField;
begin
  KeyField := GetFieldByName(FPKFieldName);

  if not Assigned(KeyField)
    then Exit;

  if KeyField.AsVariant = null
    then Exit;

  FQueryConstructor.ClearConditions;
  QueryConstructor.AddCondition(KeyField, ctEqual, KeyField.Value);
  DeleteRecords;
end;

procedure TMTableRecord.DeleteRecord(RecordID : integer);  //удаление записи с заданным ID
var
  KeyField : TMRecordField;
begin
  KeyField := GetFieldByName(FPKFieldName);

  if not Assigned(KeyField)
    then Exit;

  FQueryConstructor.ClearConditions;
  QueryConstructor.AddCondition(KeyField, ctEqual, RecordID);
  DeleteRecords;
end;

procedure TMTableRecord.DeleteRecords(FieldName : string; FieldValue : variant);
var
  RecordField : TMRecordField;
begin
  RecordField := GetFieldByName(FieldName);

  if not Assigned(RecordField)
    then Exit;

  FQueryConstructor.ClearConditions;
  QueryConstructor.AddCondition(RecordField, ctEqual, FieldValue);
  DeleteRecords;
end;

procedure TMTableRecord.DeleteRecords(FieldName1 : string; FieldValue1 : variant;
                                       FieldName2 : string; FieldValue2 : variant);
var
  RecordField1,
  RecordField2 : TMRecordField;
begin
  RecordField1 := GetFieldByName(FieldName1);
  RecordField2 := GetFieldByName(FieldName2);

  if not Assigned(RecordField1)
    then Exit;

  if not Assigned(RecordField2)
    then Exit;

  FQueryConstructor.ClearConditions;
  QueryConstructor.AddCondition(RecordField1, ctEqual, FieldValue1);
  QueryConstructor.AddCondition(RecordField2, ctEqual, FieldValue2);
  DeleteRecords;
end;

function TMTableRecord.GetMaxValue(FieldName : string) : integer;
var
  SQLQuery : String;
begin
  FCurrentRecordIndex := 0;
  FRecordsCount := 0;

  FQuery.Active := FALSE;
  ClearRecordValues;

  SQLQuery := GetMaxQueryString(FieldName);
  FQuery.SQL.Text := SQLQuery;

  FQuery.Active := TRUE;

  Result := FQuery.FieldByName('max').AsInteger;

  FQueryConstructor.ClearConditions;
end;

//////////////TMTableRecordsSSSSSSS//////////////////////////////////
constructor TMTableRecords.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMTableRecords.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;


function TMTableRecords.GetItem(ItemIndex : integer) : TMTableRecord;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMTableRecord;
end;


function TMTableRecords.GetItem(ItemTitle : string) : TMTableRecord;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMTableRecords.AddItem(Item : TMTableRecord) : TMTableRecord;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.TableName, Item) >= 0
    then Result := Item;
end;

function TMTableRecords.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


function TMTableRecords.DeleteItem(ItemIndex : integer) : boolean;
var
  RecordField : TMTableRecord;
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

procedure TMTableRecords.Reset;
var
  i : integer;
  RecordField : TMTableRecord;
begin
  for i := 0 to GetCount - 1 do
  begin
    RecordField := Items.Objects[i] as TMTableRecord;
    if Assigned(RecordField)
      then RecordField.Free;
  end;

  Items.Clear;
end;

function TMTableRecords.GetCount : integer;
begin
  Result := Items.Count;
end;

end.
