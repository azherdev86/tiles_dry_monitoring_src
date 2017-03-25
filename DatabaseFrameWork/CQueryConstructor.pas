unit CQueryConstructor;

interface

uses Classes,
     DB,
     CRecordFields,
     CConditions,
     CConditionGroups,
     CFields,
     CBrowseFields;

//Используется в качестве поля в TMTableRecords.
//Если содержит хотя бы одно условие или группу - участвует
//в формировании запросов с условиями (после WHERE)


//***************** ПРИМЕРЫ ИСПОЛЬЗОВАНИЯ ********************//
{Пример 1. Работа с ГРУППАМИ условий
Выполнится запрос:
SELECT * FROM TICKETS WHERE ((TicketID>=1) AND (TicketID<=2)) OR ((TicketID>=4) AND (TicketID<=5));
*************************************
var
  TableRecords : TMTableRecords;
  QueryConstructor : TMQueryConstructor;
begin
  TableRecords := TMTableRecords.Create('Tickets');
  try
    QueryConstructor := TableRecords.QueryConstructor;

    with QueryConstructor.AddGroupCondition(otAND, otOR) do
    begin
      AddCondition('TicketID', ctMoreEqual, 1, ftVariant);
      AddCondition('TicketID', ctLessEqual, 2, ftVariant);
    end;

    with QueryConstructor.AddGroupCondition(otAND) do
    begin
      AddCondition('TicketID', ctMoreEqual, 4, ftVariant);
      AddCondition('TicketID', ctLessEqual, 5, ftVariant);
    end;
    if TableRecords.LoadRecords > 0
      then [исполняемый код]
  finally
    TableRecords.Free;
  end;
end;

**********************************************
Пример 2. Работа с условиями БЕЗ групп
Выполнится запрос:
SELECT * FROM Tickets WHERE (TicketID>=1) AND (TicketID <=2);
**********************************************
var
  TableRecords : TMTableRecords;
  QueryConstructor : TMQueryConstructor;
begin
  TableRecords := TMTableRecords.Create('Tickets');
  try
    QueryConstructor := TableRecords.QueryConstructor;
    QueryConstructor.Operation := otAND;

    AddCondition('TicketID', ctMoreEqual, 1, ftVariant);
    AddCondition('TicketID', ctLessEqual, 2, ftVariant);


    if TableRecords.LoadRecords > 0
      then [исполняемый код]
  finally
    TableRecords.Free;
  end;
end; }
type
  TMQueryConstructor = class
    constructor Create;
    destructor Destroy; override;

  public
    //группы условий
    ConditionGroups : TMConditionGroups;
    Operation : TMOperationType; //используется только для условия, которые не входят
    //в ConditionGroups

    function GetCondition(ConditionIndex : integer) : TMCondition; overload;
    function GetCondition(FieldTitle : string) : TMCondition; overload;


    function AddCondition(RecordField : TMRecordField;
                          ConditionType : TMConditionType;
                          FieldValue : variant) : TMCondition; overload;

    function AddCondition(Field : TMField;
                          ConditionType : TMConditionType;
                          FieldValue : variant) : TMCondition; overload;

    function AddCondition(BrowseField : TMBrowseField;
                          ConditionType : TMConditionType;
                          FieldValue : variant) : TMCondition; overload;

    function AddCondition(TableName, FieldName : string;
                          ConditionType : TMConditionType;
                          FieldValue : variant): TMCondition; overload;

    function GetConditionsCount : integer;
    function GetWhereClause(IncludeWhereInQuery : boolean = true) : string;
    function AddGroupCondition(OperationIn : TMOperationType;
                               OperationAfter : TMOperationType = otNone) : TMConditionGroup;

    procedure ClearConditions;
    procedure CopyFromQueryConstructor(QueryConstructor : TMQueryConstructor);
  private
    FCounter : integer;
    Items : TStringList;
    procedure Reset;
    function AddCondition(Condition : TMCondition) : TMCondition; overload;
    function AddCondition(FieldName : string;
                          QueryText : string;
                          FieldTitle : string;
                          ConditionType : TMConditionType;
                          FieldValue : Variant;
                          FieldType : TFieldType;
                          TableName : string) : TMCondition; overload;
end;



implementation


uses Variants,
     SysUtils,
     LApplicationGlobals,
     CTableStructure,
     Dialogs;

function OperationTypeToString(OperationType : TMOperationType) : string; forward;


//********************************************
//TMQueryConstructor

constructor TMQueryConstructor.Create;
begin
  Items := TStringList.Create;
  ConditionGroups := TMConditionGroups.Create;
  Reset;
end;


destructor TMQueryConstructor.Destroy;
begin
  Reset;
  Items.Free;
  ConditionGroups.Free;
  inherited;
end;

function TMQueryConstructor.GetCondition(ConditionIndex : integer) : TMCondition;
begin
  Result := nil;

  if ConditionIndex < 0
    then Exit;

  Result := Items.Objects[ConditionIndex] as TMCondition;
end;


function TMQueryConstructor.AddCondition(Condition : TMCondition) : TMCondition;
begin
  Result := nil;

  if not Assigned(Condition)
    then Exit;

  if Items.AddObject(IntToStr(FCounter), Condition) >= 0
    then Result := Condition;

  INC(FCounter);
end;


function TMQueryConstructor.AddCondition(FieldName : string;
                                         QueryText : string;
                                         FieldTitle : string;
                                         ConditionType : TMConditionType;
                                         FieldValue : Variant;
                                         FieldType : TFieldType;
                                         TableName : string) : TMCondition;
var
  Condition : TMCondition;
begin
  Condition := TMCondition.Create;
  Condition.FieldName := FieldName;
  Condition.Value := FieldValue;
  Condition.FieldTitle := FieldTitle;
  Condition.ConditionType := ConditionType;
  Condition.FieldType := FieldType;
  Condition.TableName := TableName;
  Condition.QueryText  := QueryText;

  Result := AddCondition(Condition);
end;

function TMQueryConstructor.AddCondition(RecordField : TMRecordField;
                                         ConditionType : TMConditionType;
                                         FieldValue : variant) : TMCondition;
begin
  Result := nil;

  if not Assigned(RecordField)
    then Exit;

  Result := AddCondition(RecordField.FieldName, '', '', ConditionType, FieldValue, RecordField.FieldType, RecordField.TableName);
end;

function TMQueryConstructor.AddCondition(Field : TMField;
                                         ConditionType : TMConditionType;
                                         FieldValue : variant) : TMCondition;
begin
  Result := nil;

  if not Assigned(Field)
    then Exit;

  Result := AddCondition(Field.FieldName, '', '', ConditionType, FieldValue, Field.FieldType, Field.TableName);
end;

function TMQueryConstructor.AddCondition(BrowseField : TMBrowseField;
                                         ConditionType : TMConditionType;
                                         FieldValue : variant) : TMCondition;
begin
  Result := nil;

  if not Assigned(BrowseField)
    then Exit;


  Result := AddCondition(BrowseField.FieldName,
                         BrowseField.QueryText,
                         BrowseField.FieldTitle,
                         ConditionType,
                         FieldValue,
                         BrowseField.FieldType,
                         BrowseField.TableName);

end;

function TMQueryConstructor.AddCondition(TableName, FieldName : string;
                                         ConditionType : TMConditionType;
                                         FieldValue : variant): TMCondition;
var
  TableStructure : TMTableStructure;
  Field : TMField;
begin
  Result := nil;

  TableStructure := ApplicationDataBaseStructure.GetTable(TableName);

  if not Assigned(TableStructure)
    then Exit;

  Field := TableStructure.GetField(FieldName);

  Result := AddCondition(Field, ConditionType, FieldValue);
end;


function TMQueryConstructor.GetCondition(FieldTitle : string) : TMCondition;
var
  i : integer;
  ConditionsCount : integer;
  Condition : TMCondition;
begin
  Result := nil;

  ConditionsCount := GetConditionsCount;

  for i := 0 to ConditionsCount - 1 do
  begin
    Condition := GetCondition(i);

    if not Assigned(Condition)
      then Continue;

    if (Condition.FieldTitle = FieldTitle)
      then
        begin
          Result := Condition;
          break;
        end;
  end;        
end;


procedure TMQueryConstructor.Reset;
var
  i, count : integer;
  Condition : TMCondition;
begin
  count := GetConditionsCount;

  for i := 0 to count - 1 do
  begin
    Condition := Items.Objects[i] as TMCondition;
    if Assigned(Condition)
      then Condition.Free;
  end;

  Items.Clear;
  FCounter := 0;
  Operation := otNone;
  ConditionGroups.Reset;
end;

function TMQueryConstructor.GetConditionsCount : integer;
begin
  Result := Items.Count;
end;

procedure TMQueryConstructor.ClearConditions;
begin
  Reset;
end;

procedure TMQueryConstructor.CopyFromQueryConstructor(QueryConstructor : TMQueryConstructor);
var
  i : integer;
  ConditionsCount : integer;
  NewCondition, OldCondition : TMCondition;
begin
  ConditionsCount := QueryConstructor.GetConditionsCount;
  for i := 0 to ConditionsCount - 1 do
  begin
    OldCondition := QueryConstructor.GetCondition(i);
    if not Assigned(OldCondition)
      then Break;

    NewCondition := TMCondition.Create;
    NewCondition.CopyFromCondtion(OldCondition);
    AddCondition(NewCondition);
  end;

  Operation := QueryConstructor.Operation;

  ConditionGroups.CopyFromConditionGroups(QueryConstructor.ConditionGroups);
end;


function TMQueryConstructor.GetWhereClause(IncludeWhereInQuery : boolean = true) : string;
var
  i, j : integer;
  Condition : TMCondition;
  Group : TMConditionGroup;
  ConditionsCount : integer;
  ConditionsGroupCount : integer;
  GroupsCount : integer; //количество групп в запросе
begin
  Result := '';

  //Создаем переменные для того чтобы уменишьить количество
  //обращений к свойствам объектов
  ConditionsCount := GetConditionsCount;
  GroupsCount := ConditionGroups.GetCount;

  //Если нет ни условия, ни группы условий то завершаем работу подпрограммы
  if (ConditionsCount = 0) and
     (GroupsCount = 0)
    then Exit;

  if IncludeWhereInQuery
    then Result := 'WHERE ';

  //ПОСТРОЕНИЕ СТРОКИ WHERE ДЛЯ ЗАПРОСА БЕЗ!!!! ГРУПП УСЛОВИЙ
  //WHERE (A = 5) AND (B = 6) AND (C = "C")
  //Здесь AND - Operation - одна для всех условий. Если AND, то везде AND
  //Если OR, то везде OR и т.д.
  for i := 0 to ConditionsCount - 1 do
    begin
      Condition := GetCondition(i);
      if not Assigned(Condition)
        then Continue;

      Result := Result + '(' + Condition.GetConditionString + ')';
      //для всех условий кроме последнего добавляем после них
      //Operation : (Условие1) Operation (Условие2) Operation (Условие3)
      if i < (ConditionsCount - 1)
        then Result := Result + OperationTypeToString(Operation);
    end;


  //ПОСТРОЕНИЕ СТРОКИ WHERE ДЛЯ ЗАПРОСА С!!! ГРУППАМИ УСЛОВИЙ
  for i := 0 to GroupsCount - 1 do
    begin
      Group := ConditionGroups.GetItem(i);
      if not Assigned(Group)
        then Continue;

      ConditionsGroupCount := Group.GetConditionsCount;

      if ConditionsCount <> 0
        then Result := Result + OperationTypeToString(otAND) + '('
        else Result := Result + '(';

      for j := 0 to ConditionsGroupCount - 1 do
      begin
        Condition := Group.GetCondition(j);
        if not Assigned(Condition)
          then Continue;

        Result := Result + Condition.GetConditionString;

        if j < (ConditionsGroupCount - 1)
          then Result := Result + OperationTypeToString(Group.OperationIn);
      end;
      Result := Result + ')';
      //для всех условий кроме последнего добавляем после них
      //Operation : (Условие1) Operation (Условие2) Operation (Условие3)
      if i < (GroupsCount - 1)
        then Result := Result + OperationTypeToString(Group.OperationAfter);
    end;
end;

function TMQueryConstructor.AddGroupCondition(OperationIn : TMOperationType;
                               OperationAfter : TMOperationType = otNone) : TMConditionGroup;
var
  ConditionGroup : TMConditionGroup;
begin
  ConditionGroup := TMConditionGroup.Create;

  ConditionGroup.OperationIn := OperationIn;
  ConditionGroup.OperationAfter := OperationAfter;

  Result := ConditionGroups.AddItem(ConditionGroup);
end;

//*****************************************************************
///*************ОТДЕЛЬНЫЕ ПРОЦЕДУРЫ И ФУНКЦИИ *********************
//*****************************************************************

function OperationTypeToString(OperationType : TMOperationType) : string;
begin
  case OperationType of
    otNone: Result := '';
    otXOR:  Result := ' XOR ';
    otOR:   Result := ' OR ';
    otAND:  Result := ' AND ';
  end;
end;

end.
