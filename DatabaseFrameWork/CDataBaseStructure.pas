{*****************************************************************************************
******************************************************************************************
***************В ЭТОМ МОДУЛЕ УКАЗЫВАЕТСЯ ВСЯ СТРУКТУРА ДАННЫХ БАЗЫ ДАННЫХ*****************
******************************************************************************************
******************************************************************************************}
unit CDataBaseStructure;

interface

uses CTableStructure, //DBTables
     ZConnection,
     ZDataset;

type
  TMDatabaseType =(dtNone, dtMySQL, dtFireBird);

type
  TMDatabaseStructure = class
    constructor Create(DBConn : TZConnection; NewDataBaseType : TMDatabaseType = dtMySQL);
    destructor Destroy; override;


  public
    Tables : TMTables;
    DataBaseName : string;
    AliasName : string;
    DatabaseType : TMDatabaseType;
    
    //***********************************************************************************
    function LoadStructure() : boolean;  //в этом методе указывается вся структура данных
    //***********************************************************************************
    procedure Reset;
    function CheckTableNames(var OutMessage : string) : boolean;
    function AddTable(TableName, PKFieldName : string) : TMTableStructure;
    function GetDBType() : TMDatabaseType;
    function GetTable(TableName : string) : TMTableStructure; overload;
    function GetTable(Index : integer) : TMTableStructure; overload;

  private
    FDBConn : TZConnection;

    function CheckTableNamesMySQL(var OutMessage : string) : boolean;
    function CheckTableNamesFireBird(var OutMessage : string) : boolean;
  end;

implementation

uses DB,
     Dialogs,
     SysUtils,
     Classes,
     CFields;

function TMDatabaseStructure.LoadStructure() : boolean;
var
  OutMessage : string;
begin
  Result := FALSE;

  with AddTable('Sensors', 'SensorId') do
  begin
    AddField('SensorPosition', ftString);
    AddField('ConveyorNumber', ftInteger);
    AddField('SectionNumber',  ftInteger);
    AddField('BoxNumber',      ftInteger);
  end;

  with AddTable('TempValues', 'TempValueId') do
  begin
    AddField('TempValue', ftFloat);
    AddField('TempTime',  ftDateTime);

    AddForeignKey('SensorId', ftInteger, 'Sensors');
  end;

  with AddTable('EventLogs', 'EventLogId') do
  begin
    AddField('EventLogType',    ftString);
    AddField('EventLogDetails', ftString);
    AddField('EventLogTime',    ftDateTime);
  end;

  with AddTable('Messages', 'MessageId') do
  begin
    AddField('MessageType',             ftString);
    AddField('MessageRecievedTime',     ftDateTime);
    AddField('MessageRecievedPartTime', ftDateTime);
    AddField('MessageCreationTime',     ftDateTime);
    AddField('MessageSentTime',         ftDateTime);
    AddField('MessageDelieveredTime',   ftDateTime);
    AddField('MessageState',            ftString);
    AddField('MessageError',            ftString);
    AddField('MessageBytes',            ftString);
    AddField('MessageUid',              ftString);
  end;


  if not CheckTableNames(OutMessage)
    then
      begin
        ShowMessage(OutMessage);
        Exit;
      end
    else
      Result := TRUE;
end;

function TMDatabaseStructure.AddTable(TableName, PKFieldName : string) : TMTableStructure;
var
  Table : TMTableStructure;
  PKField : TMField;
begin
  Table := TMTableStructure.Create;
  try
    Table.TableName := TableName;
    Table.PKFieldName := PKFieldName;

    PKField := TMField.Create;
    PKField.TableName := TableName;
    PKField.FieldName := PKFieldName;
    PKField.FieldType := ftVariant;

    if not Assigned(Table.Fields.AddItem(PKField))
      then PKField.Free;


    
  finally
    Result := Tables.AddItem(Table);
  end;
end;

function TMDatabaseStructure.GetTable(TableName : string) : TMTableStructure;
begin
  Result := Tables.GetItem(TableName);
end;

function TMDatabaseStructure.GetTable(Index : integer) : TMTableStructure;
begin
  Result := Tables.GetItem(Index);
end;


constructor TMDatabaseStructure.Create(DBConn : TZConnection; NewDataBaseType : TMDatabaseType = dtMySQL);
begin
  DatabaseType := NewDataBaseType;
  FDBConn := DBConn;
  Tables := TMTables.Create;
  Reset;
end;

destructor TMDatabaseStructure.Destroy;
begin
  Reset;
  Tables.Free;
  inherited;
end;


procedure TMDatabaseStructure.Reset;
begin
  Tables.Reset;

  DataBaseName := '';
  AliasName := '';
  DatabaseType := dtNone; 
end;

function TMDatabaseStructure.GetDBType() : TMDatabaseType;
begin
  Result := DatabaseType;
end;

function TMDatabaseStructure.CheckTableNames(var OutMessage : string) : boolean;
begin
  Result := False;
  OutMessage := '';
  if not Assigned(FDBConn)
    then Exit;          

  case DatabaseType of
    dtMySQL   : Result := CheckTableNamesMySQL(OutMessage);
    dtFireBird: Result := CheckTableNamesFireBird(OutMessage);
  end;   
end;

function TMDataBaseStructure.CheckTableNamesMySQL(var OutMessage: string) : boolean;
var
  Query : TZQuery;
  Table : TMTableStructure;
  i, count : integer;
  ListOfTable : array of boolean;
begin
  Result := False;

  count := Tables.GetCount;
  SetLength(ListOfTable, count);

  for i := 0 to count - 1 do
    ListOfTable[i] := FALSE;
    
  Query := TZQuery.Create(FDBConn);
  try                                     
    Query.SQL.Text := 'SHOW TABLES';
    Query.Active := TRUE;
          
    while not Query.Eof do
      begin
        count := Tables.GetCount;
        
        for i := 0 to count - 1 do
        begin
          Table := Tables.GetItem(i);
          if UpperCase(Table.TableName) = UpperCase(Query.Fields[0].AsString)
            then
              begin
                ListOfTable[i] := TRUE;
                break;
              end;
        end;
        Query.Next;
      end;

    Result := TRUE;

    count := Length(ListOfTable);
    
    for i := 0 to count - 1 do
    begin
      Result := Result AND ListOfTable[i];
      if not ListOfTable[i]
        then OutMessage := OutMessage + Tables.GetItem(i).TableName + ', ';
    end;    
  finally
    Query.Active := FALSE;
    Query.Free;

    if not Result
      then OutMessage := 'В базе данных отсутствуют следующие таблицы: ' + sLineBreak +
                         OutMessage;
  end;
end;
      
function TMDatabaseStructure.CheckTableNamesFireBird(var OutMessage : string) : boolean;
var
  Strings : TStrings;
  i, j,
  tables_count, strings_count,
  list_of_table_length : integer;
  ListOfTable : array of boolean;
  Table : TMTableStructure;
begin
  Result := False;

  tables_count := Tables.GetCount;
  
  SetLength(ListOfTable, tables_count);
  for i := 0 to tables_count - 1 do
    ListOfTable[i] := FALSE;

  Strings := TStringList.Create;
  try
    FDBConn.GetTableNames('',Strings);

    strings_count := Strings.Count;

    for i := 0 to strings_count - 1 do
      for j := 0 to tables_count - 1 do
      begin
        Table := Tables.GetItem(j);
        if UpperCase(Table.TableName) = UpperCase(Strings[i])
          then
            begin
              ListOfTable[j] := TRUE;
              break;
            end;
      end;

    Result := TRUE;

    list_of_table_length := Length(ListOfTable);

    for i := 0 to list_of_table_length - 1 do
    begin
      Result := Result AND ListOfTable[i];
      if not ListOfTable[i]
        then OutMessage := OutMessage + Tables.GetItem(i).TableName + ', ';
    end;   
    
  finally
    Strings.Free;
    if not Result
      then OutMessage := 'В базе данных отсутствуют следующие таблицы: ' + sLineBreak +
                         OutMessage;
  end;
    
end;

end.
