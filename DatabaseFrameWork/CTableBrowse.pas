unit CTableBrowse;

interface

uses Classes,
     CBrowseFields,
     CSortingFields,
     CQueryConstructor,
     CConditions,
     CFields,
     CTableStructure,
     DB,
     variants;

type
  TMTableBrowses = class; //forward declaration
  TMTableBrowse = class
    constructor Create(tblName : string); overload; virtual;
    destructor Destroy; override;
  private
    FBrowseFields  : TMBrowseFields;  //список полей, которые будут отображены в таблице
    FSortingFields : TMSortingFields;  //список полей, по которым осуществляется сортировка
    FGroupingFields   : TMBrowseFields;  //список полей, по которым осуществляется группировка
    FTableName : string;              //имя таблицы
    FUserFieldsOrder : boolean;       //ручная установка порядка следования столбцов в таблице (да/нет)

    //ссылки, удалять и очищать не нужно
    FTableStructureRef : TMTableStructure;  //ссылка на структуру соответствующей таблицы
    JoinedTables : TMTableBrowses;          //список присоединенных таблиц

    FPickedUpFieldTitle : string; //Поле, по которому осуществляется объединение таблиц
    //оператором UNION. Одно множество - где PickUpedField = 0, а другое где PickUpedField = 1
    FCheckFieldTitle : string; //Полек, которое используется в качестве CheckBox = 0 не выбрано, 1 - выбрано
    procedure Reset;
    procedure GetJoinedBrowseFields(var OutBrowseFields : TMBrowseFields); //!!!РЕКУРСИВНАЯ!!!

 //   procedure GetJoinedBrowseFields(JoinedBrowseFields :
  public
    BrowseName : string;
    DefaultQueryConstructor,  //Задается в методе
    UserQueryConstructor : TMQueryConstructor;
    //Копирование объекта
    procedure CopyFromTableBrowse(TableBrowse : TMTableBrowse);
    procedure GetAllBrowseFields(var OutBrowseFields : TMBrowseFields);

    //******************************************************************
    //************ ОСНОВНОЙ МЕТОД КЛАССА********************************
    //******************************************************************
    function GetSelectQuery : string; //запрос Select со всеми настройками
    //******************************************************************

    //******************************************************************
    //*************ЗАПРОС ВСЕХ УНИКАЛЬНЫХ ЗНАЧЕНИЙ ОПРЕДЕЛЕННОГО ПОЛЯ***
    //******************************************************************
    function GetDistinctValuesQuery(FieldTitle : string) : string; overload;//нужен для создания фильтров
    function GetDistinctValuesQuery(BrowseField : TMBrowseField) : string; overload;//нужен для создания фильтров

    //******************************************************************
    //************ РАВОТА С ПОЛЯМИ, ОТОБРАЖАЕМЫМИ В ТАБЛИЦАХ************
    //******************************************************************

    //Добавление полей для отображения в TableBrowse

    function AddColorField(FieldName, FieldTitle: string;
      BrowseFieldType: TMBrowseFieldTypes = bftColorText): TMBrowseField;

    function AddBrowseField(FieldName: string; FieldTitle : string;
      BrowseFieldType: TMBrowseFieldTypes = bftData): TMBrowseField; overload;

    function AddBrowseField(FieldName: string; FieldTitle: string;
      FieldVisible: boolean): TMBrowseField; overload;

    function AddDataFunctionField(FieldName: string; FieldTitle: string;
      GetFieldText: TMGetFieldTextFunction) : TMBrowseField;

    //добавление поля, которое в себе содержит SQL Text
    function AddCalculatedField(QueryText, FieldTitle: string;
      FieldType: TFieldType) : TMBrowseField;

    //добавление поля, которое в себе содержит SQL Text
    function AddCalculatedFunctionField(QueryText, FieldTitle: string;
      FieldType: TFieldType; GetFieldText: TMGetFieldTextFunction): TMBrowseField;

    function AddGraphicField(FieldTitle: string;
      FieldType: TFieldType = ftInteger): TMBrowseField;

    function AddAgregateField(QueryText, FieldTitle: string;
      FieldType: TFieldType; FieldVisible: boolean = true): TMBrowseField;

    function DeleteBrowseField(FieldTitle: string): boolean;

    //Количество полей для просмотра
    function GetBrowseFieldsCount() : integer;

    //Доступ к к полям просмотра по имени и по индексу
    function GetBrowseField(FieldTitle : string) : TMBrowseField; overload;

    function GetBrowseField(FieldIndex : integer) : TMBrowseField; overload;

    //******************************************************************
    //************ РАВОТА С ПРИСОЕДИНЕННЫМИ ТАБЛИЦАМИ*******************
    //******************************************************************

    //Добавление присоединенных таблиц
    function AddJoinedTable(TableName : string) : TMTableBrowse;
    //Доступ к присоединенным  таблицам по имени и по индексу
    function GetJoinedTable(TableName : string) : TMTableBrowse; overload;
    function GetJoinedTable(TableIndex : integer) : TMTableBrowse; overload;

    //******************************************************************
    //************ РАВОТА С ПОЛЯМИ ДЛЯ СОРТИРОВКИ***********************
    //******************************************************************
    //Добавление полей сортировки
    function AddSortingField(FieldTitle : string; revSorting : boolean = false) : TMSortingField; overload;
    //Использование этой функции позволяет задавать сортировку даже по тем полям,
    //которые отсутствуют в списке перечисленных FBrowseFields
    function AddSortingField(FieldName, TableName : string; RevSorting : boolean = false) : TMBrowseField; overload;
    function GetSortingField(Index : integer) : TMSortingField;    overload;
    function GetSortingField(FieldTitle : string) : TMSortingField; overload;
    function SetSortingField(FieldTitle : string) : TMSortingField;
    function DeleteSortingField(FieldTitle : string) : boolean;
    procedure ClearSortingFields;
    function GetSortingFieldsCount() : integer;

    //******************************************************************
    //************ РАВОТА С ПОЛЯМИ ДЛЯ ГРУППИРОВКИ***********************
    //******************************************************************
    //Добавление полей сортировки
    function AddGroupingField(BrowseTitle : string) : TMBrowseField;
    procedure ClearGroupingFields;

    //******************************************************************
    //************ МЕТОДЫ, ДЛЯ РАБОТЫ С УСЛОВИЯМИ***********************
    //******************************************************************
    function AddDefaultCondition(FieldTitle : string;
                                 ConditionType : TMConditionType;
                                 FieldValue : variant) : TMCondition; overload;

    function AddDefaultCondition(FieldName, TableName : string;
                                 ConditionType : TMConditionType;
                                 FieldValue : variant) : TMCondition; overload;

    function GetDefaultCondition(FieldTitle : string) : TMCondition;

    function AddUserCondition(FieldTitle : string;
                              ConditionType : TMConditionType;
                              FieldValue : variant) : TMCondition; overload;

    function AddUserCondition(FieldName, TableName : string;
                              ConditionType : TMConditionType;
                              FieldValue : variant) : TMCondition; overload;

    //******************************************************************
    //************ МЕТОДЫ,ДЛЯ РАБОТЫ С ПОЛЯМИ ДЛЯ ПРИКРЕПЛЕНИЯ**********
    //******************************************************************
    function SetPickUpedField(FieldTitle : string) : TMBrowseField;

    procedure DeletePickUpedField();

    //******************************************************************
    //************ МЕТОДЫ,ДЛЯ РАБОТЫ С CHECKBOX полями******************
    //******************************************************************
    function SetCheckField(FieldTitle : string) : TMBrowseField;
    procedure DeleteCheckField();


    //******************************************************************
    //************ МЕТОДЫ, ОБЩИЕ ДЛЯ ВСЕХ ПОЛЕЙ*************************
    //******************************************************************
    //возвращает поле BrowseField из FBrowseFields, а также из любой из
    //присоединенных таблиц (JoinedTables)
    function GetAnyField(FieldTitle : string) : TMBrowseField;
    function GetJoinedField(FieldTitle : string) : TMBrowseField;  //!!!РЕКУРСИЯ!!!!
    //Установка порядка следования для поля
    procedure SetFieldOrder(BrowseTitle : string; FieldOrder : integer);
    function GetMaxFieldOrder() : integer;
    //Установка ширины столбца в таблице
    procedure SetFieldWidth(BrowseTitle : string; FieldWidth : integer = 0);

    //******************************************************************
    //************ МЕТОДЫ, ДЛЯ РАБОТЫ С ФИЛЬТРАМИ ПО ВСЕМ ПОЛЯМ*********
    //******************************************************************
    procedure SetAllFieldFilter(FilterValue : string);

    //******************************************************************
    //************ МЕТОДЫ, ДЛЯ РАБОТЫ С ПОДСКАЗКАМИ (HINTS)*************
    //******************************************************************
    procedure AddFieldHint(FieldTitle, FieldValue, HintText : string; ConditionType : TMConditionType = ctNone);
    procedure SetFieldHint(FieldTitle, HintText : string);

    //******************************************************************
    //************ МЕТОДЫ, ДЛЯ РАБОТЫ С ПОДСКАЗКАМИ (HINTS)*************
    //******************************************************************
    procedure SetFieldTitleVisible(FieldTitle : string; Visible : boolean = false);


    //******************************************************************
    //************МЕТОДЫ ДЛЯ РАБОТЫ С ИЗОБРАЖЕНИЯМ**********************
    //******************************************************************
    procedure AddFieldImage(FieldTitle : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1); overload;
    procedure AddFieldImage(FieldTitle, FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone); overload;
    procedure AddReferense(FieldTitle : string);
    procedure AddProgressBar(FieldTitle, CurrentProgressFieldTitle, MaxValueFieldTitle : string);
    procedure AddExtendedProgressBar(FieldTitle, CurrentProgressFieldTitle, ExtendedProgressFieldTitle, MaxValueFieldTitle : string);

    //******************************************************************
    //************ДОСТУП К ПЕРВОМУ И ПОСЛЕДНЕМУ ВИДИМЫМ ПОЛЯМ **********
    //******************************************************************
    //Возвращает название первого видимого поля
    function GetFirstVisibleFieldTitle : string;

    //Возвращает название последнего видимого поля
    function GetLastVisibleFieldTitle : string;
  public
    property UserFieldsOrder : boolean read FUserFieldsOrder write FUserFieldsOrder;
    property QueryConstructor : TMQueryConstructor read UserQueryConstructor write UserQueryConstructor;
    property TableStructure : TMTableStructure read FTableStructureRef;
    property PickUpedFieldTitle : string read FPickedUpFieldTitle;
    property CheckFieldTitle : string read FCheckFieldTitle;
    property TableName : string read FTableName;
    //property BrowseFields : TMBrowseFields read FBrowseFields write FBrowseFields;
  protected
    //******************************************************************
    //************ МЕТОДЫ, ДЛЯ ФОРМИРОВАНИЯ ЗАПРОСА SELECT**************
    //******************************************************************
    //Список полей, идущий после слова SELECT и до слова FROM
    function GetBrowseFieldsString()     : string;
    //Список полей сортировки. Следует в конце запроса. Если полей для сортировки
    //явно не указано, сортировка выполняется по первичному ключи таблицы
    function GetSortingFieldsString()    : string;
    //Список полей, по которым выполняется группировка (поле должно находится в списек Browse полей )
    function GetGroupingFieldsString()    : string;
    //Список полей из ПРИСОЕДИНЕННЫХ таблиц после слова SELECT и до слова FROM
    function GetJoinedFieldsString() : string;  //!!! РЕКУРСИВНАЯ !!!
    //Список присоединеных таблиц. Следует сразу после FROM TableName
    function GetJoinedTablesString() : string;  //!!! РЕКУРСИВНАЯ !!!
    //Список полей, идущий после слова SELECT и до слова FORM с учетом порядка следования
    function GetUserOrderBrowseFieldsString() : string;
    //Список условия запроса
    function GetWhereString() : string;
    //Текст запроса, который содержится после поля FROM (включает в себя)
    //GroupingFields, JoinedTables
    function GetAfterFromString : string; virtual;
     //******************************************************************
    //************ МЕТОДЫ, ДЛЯ РАБОТЫ С УСЛОВИЯМИ************************
    //*******************************************************************
    function AddCondition(ChoosenQueryConstructor : TMQueryConstructor;
                          FieldTitle : string;
                          ConditionType : TMConditionType;
                          FieldValue : variant) : TMCondition; overload;

    function AddCondition(ChoosenQueryConstructor : TMQueryConstructor;
                          FieldName, TableName : string;
                          ConditionType : TMConditionType;
                          FieldValue : variant) : TMCondition; overload;
 end;

  TMTableBrowses = class
    constructor Create;
    destructor Destroy; override;

  public
    procedure CopyFromTableBrowses(TableBrowses : TMTableBrowses);
    function GetItem(ItemIndex : integer) : TMTableBrowse; overload;
    function GetItem(ItemTitle : string) : TMTableBrowse; overload;

    function AddItem(Item : TMTableBrowse) : TMTableBrowse;
    function DeleteItem(ItemTitle : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;
  end;


implementation

uses  LApplicationGlobals,
      Dialogs,
      SysUtils,
      CDataBaseStructure,
      CConditionGroups,
      CRecordFields;


constructor TMTableBrowse.Create(tblName : string);
begin
  FBrowseFields := TMBrowseFields.Create;
  FSortingFields := TMSortingFields.Create;
  FGroupingFields := TMBrowseFields.Create;

  JoinedTables := TMTableBrowses.Create;
  DefaultQueryConstructor := TMQueryConstructor.Create;
  UserQueryConstructor    := TMQueryConstructor.Create;

  Reset;

  FTableName := tblName;

  //В деструкторе описывать не нужно
  FTableStructureRef := ApplicationDataBaseStructure.GetTable(FTableName);
end;

destructor TMTableBrowse.Destroy;
begin
  FBrowseFields.Free;
  FSortingFields.Free;
  FGroupingFields.Free;

  JoinedTables.Free;

  DefaultQueryConstructor.Free;
  UserQueryConstructor.Free;

  inherited;
end;

procedure TMTableBrowse.CopyFromTableBrowse(TableBrowse : TMTableBrowse);
begin
  if not Assigned(TableBrowse)
    then Exit;

  FBrowseFields.CopyFromBrowseFields(TableBrowse.FBrowseFields);
  FSortingFields.CopyFromSortingFields(TableBrowse.FSortingFields);
  FGroupingFields.CopyFromBrowseFields(TableBrowse.FGroupingFields);

  FTableName := TableBrowse.FTableName;
  FUserFieldsOrder := TableBrowse.FUserFieldsOrder;

  //ссылки, удалять и очищать не нужно
  FTableStructureRef := TableBrowse.FTableStructureRef;

  JoinedTables.CopyFromTableBrowses(TableBrowse.JoinedTables);

  FPickedUpFieldTitle := TableBrowse.FPickedUpFieldTitle;

  FCheckFieldTitle := TableBrowse.FCheckFieldTitle;

  BrowseName := TableBrowse.BrowseName;
  DefaultQueryConstructor.CopyFromQueryConstructor(TableBrowse.DefaultQueryConstructor);
  UserQueryConstructor.CopyFromQueryConstructor(TableBrowse.UserQueryConstructor);
end;

procedure TMTableBrowse.Reset;
begin
  FBrowseFields.Reset;
  FSortingFields.Reset;
  FGroupingFields.Reset;
  JoinedTables.Reset;

  FUserFieldsOrder := FALSE;
  FTableName := '';

  BrowseName := '';
  QueryConstructor.ClearConditions;
  DefaultQueryConstructor.ClearConditions;
  DefaultQueryConstructor.Operation := otAND;
end;


procedure TMTableBrowse.GetJoinedBrowseFields(var OutBrowseFields : TMBrowseFields); //!!!РЕКУРСИВНАЯ!!!
var
  i,
  JoinedTablesCount : integer;
  JoinedTable : TMTableBrowse;
begin
  if not Assigned(OutBrowseFields)
    then Exit;

  JoinedTablesCount := JoinedTables.GetCount;

  for i := 0 to JoinedTablesCount - 1 do
  begin
    JoinedTable := GetJoinedTable(i);
    if not Assigned(JoinedTable)
      then Continue;

    OutBrowseFields.AddItems(JoinedTable.FBrowseFields);

    JoinedTable.GetJoinedBrowseFields(OutBrowseFields);  //!!!Рекурсия!!!
  end;
end;

procedure TMTableBrowse.GetAllBrowseFields(var OutBrowseFields : TMBrowseFields);
begin
  if not Assigned(OutBrowseFields)
    then Exit;

  OutBrowseFields.AddItems(FBrowseFields);
  GetJoinedBrowseFields(OutBrowseFields);
end;


function TMTableBrowse.GetSelectQuery : string; //запрос Select
var
  BrowseFieldsClause : string; // FieldName1 as "FieldTitle1", ..., FieldNameN as "FieldTitleN"
  JoinedFieldsClause : string;
  AfterFromClause : string;
begin
  Result := '';

  //Если установлен пользовательский порядок следования полей
  if FUserFieldsOrder
    then
      begin
        BrowseFieldsClause := GetUserOrderBrowseFieldsString;
        JoinedFieldsClause := '';
      end
    else
      begin
        BrowseFieldsClause := GetBrowseFieldsString;
        JoinedFieldsClause := GetJoinedFieldsString;
      end;


    AfterFromClause := GetAfterFromString;

    Result := 'SELECT ' + BrowseFieldsClause +
              JoinedFieldsClause +
              ' FROM ' + FTableName + ' ' +
              AfterFromClause;
end;




procedure TMTableBrowse.ClearSortingFields;
begin
  FSortingFields.Reset;
end;

procedure TMTableBrowse.ClearGroupingFields;
begin
  FGroupingFields.Reset;
end;

function TMTableBrowse.GetSortingFieldsCount() : integer;
begin
  Result := FSortingFields.GetCount;
end;


function TMTableBrowse.AddDefaultCondition(FieldTitle : string;
                                           ConditionType : TMConditionType;
                                           FieldValue : variant) : TMCondition;
begin
  Result := AddCondition(DefaultQueryConstructor, FieldTitle, ConditionType, FieldValue);
end;

function TMTableBrowse.AddDefaultCondition(FieldName, TableName : string;
                                           ConditionType : TMConditionType;
                                           FieldValue : variant) : TMCondition;
begin
  Result := AddCondition(DefaultQueryConstructor, FieldName, TableName, ConditionType, FieldValue);
end;

function TMTableBrowse.GetDefaultCondition(FieldTitle : string) : TMCondition;
begin
  Result := DefaultQueryConstructor.GetCondition(FieldTitle);
end;

function TMTableBrowse.AddUserCondition(FieldTitle : string;
                                        ConditionType : TMConditionType;
                                        FieldValue : variant) : TMCondition;
begin
  Result := AddCondition(UserQueryConstructor, FieldTitle, ConditionType, FieldValue);
end;

function TMTableBrowse.AddUserCondition(FieldName, TableName : string;
                                        ConditionType : TMConditionType;
                                        FieldValue : variant) : TMCondition;
begin
  Result := AddCondition(UserQueryConstructor, FieldName, TableName, ConditionType, FieldValue);
end;

//******************************************************************
    //************ МЕТОДЫ,ДЛЯ РАБОТЫ С ПОЛЯМИ ДЛЯ UNION*****************
    //******************************************************************
function TMTableBrowse.SetPickUpedField(FieldTitle : string) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  Result := nil;

  BrowseField := GetAnyField(FieldTitle);

  if not assigned(BrowseField)
    then Exit;

  FPickedUpFieldTitle := FieldTitle;

  Result := BrowseField;

  AddSortingField(FieldTitle, true);
end;

function TMTableBrowse.SetCheckField(FieldTitle : string) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  Result := nil;

  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  FCheckFieldTitle := FieldTitle;

  Result := BrowseField;
end;

procedure TMTableBrowse.DeletePickUpedField();
begin
  FPickedUpFieldTitle := '';
end;

procedure TMTableBrowse.DeleteCheckField();
begin
  FCheckFieldTitle := '';
end;


function TMTableBrowse.GetDistinctValuesQuery(FieldTitle : string) : string; //нужен для создания фильтров
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  Result := GetDistinctValuesQuery(BrowseField);
end;

function TMTableBrowse.GetDistinctValuesQuery(BrowseField : TMBrowseField) : string; //нужен для создания фильтров
var
  AfterFromClause : string;
begin
  AfterFromClause := GetAfterFromString;
  Result := 'SELECT DISTINCT ' + BrowseField.TitleString + ' FROM ' +
            FTableName + ' ' + AfterFromClause;
end;

function TMTableBrowse.AddBrowseField(FieldName : string; FieldTitle : string; BrowseFieldType : TMBrowseFieldTypes = bftData) : TMBrowseField;
var
  BrowseField : TMBrowseField;
  Field : TMField;
begin
  Result := nil;

  Field := FTableStructureRef.GetField(FieldName);

  if not Assigned(Field)
    then Exit;

  BrowseField := TMBrowseField.Create();

  BrowseField.CopyFromField(Field);

  BrowseField.BrowseFieldType := BrowseFieldType;
  BrowseField.FieldTitle := FieldTitle;

  Result := FBrowseFields.AddItem(BrowseField);
end;

function TMTableBrowse.GetAnyField(FieldTitle : string) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  BrowseField := FBrowseFields.GetItem(FieldTitle);

  if Assigned(BrowseField)
    then
      begin
        Result := BrowseField;
        Exit;
      end;

  //Если поля не оказалось в FBrowseField ищем его в присоединенных таблицах
    Result := GetJoinedField(FieldTitle);
end;

function TMTableBrowse.GetJoinedField(FieldTitle : string) : TMBrowseField; //!!!РЕКУРСИВНАЯ!!!
var
  i : integer;
  JoinedTable : TMTableBrowse;
  JoinedTablesCount : integer;
  JoinedField : TMBrowseField;
begin
  Result := nil;

  JoinedTablesCount := JoinedTables.GetCount;

  for i := 0 to JoinedTablesCount - 1 do
  begin
    JoinedTable := GetJoinedTable(i);

    if not Assigned(JoinedTable)
      then Continue;

    JoinedField := JoinedTable.GetBrowseField(FieldTitle);

    if Assigned(JoinedField)
      then
        begin
          Result := JoinedField;
          Exit;
        end
      else Result := JoinedTable.GetJoinedField(FieldTitle); //!!!РЕКУРСИЯ!!!
  end;
end;

procedure TMTableBrowse.SetFieldOrder(BrowseTitle : string; FieldOrder : integer);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(BrowseTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.FieldOrder := FieldOrder;
end;

function TMTableBrowse.GetMaxFieldOrder() : integer;
var
  OutBrowseFields : TMBrowseFields;
  BrowseField : TMBrowseField;
  FieldsCount : integer;
  i : integer;
begin
  Result := 0;

  if not UserFieldsOrder
    then Exit;
  

  OutBrowseFields := TMBrowseFields.Create;
  try
    GetAllBrowseFields(OutBrowseFields);

    FieldsCount := OutBrowseFields.GetCount;

    for i := 0 to FieldsCount - 1 do
    begin
      BrowseField := OutBrowseFields.GetItem(i);
      if not Assigned(BrowseField)
        then break;

      if BrowseField.FieldOrder > Result
        then Result := BrowseField.FieldOrder;
    end;                                      
  finally
    OutBrowseFields.DestroyWithoutElementsFree;
  end;
end;

procedure TMTableBrowse.SetFieldWidth(BrowseTitle : string; FieldWidth : integer = 0);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(BrowseTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.FieldWidth := FieldWidth;
end;

//******************************************************************
//************ МЕТОДЫ, ДЛЯ РАБОТЫ С ФИЛЬТРАМИ ПО ВСЕМ ПОЛЯМ*********
//******************************************************************
procedure TMTableBrowse.SetAllFieldFilter(FilterValue : string);
var
  i,
  FieldsCount : integer;
  tmpBrowseFields : TMBrowseFields;
  BrowseField : TMBrowseField;
begin
  UserQueryConstructor.ClearConditions;


  tmpBrowseFields := TMBrowseFields.Create;
  try
    GetAllBrowseFields(tmpBrowseFields);

    FieldsCount := tmpBrowseFields.GetCount;

    with QueryConstructor.AddGroupCondition(otOR) do
      for i := 0 to FieldsCount - 1 do
      begin
        BrowseField := tmpBrowseFields.GetItem(i);

        if (BrowseField.Visible) AND
           (BrowseField.BrowseFieldType <> bftGraphic) AND
  //         (BrowseField.BrowseFieldType <> bftCalculated) AND
           (BrowseField.BrowseFieldType <> bftAgregate) AND
           (BrowseField.FieldType <> ftDate) AND
           (BrowseField.FieldType <> ftTime) AND
           (BrowseField.FieldType <> ftDateTime)
          then
            AddCondition(BrowseField, ctLike, FilterValue);
      end;
  finally
    tmpBrowseFields.DestroyWithoutElementsFree;
    QueryConstructor.ConditionGroups.GetItem(0);
  end;

end;

//******************************************************************
//************ МЕТОДЫ, ДЛЯ РАБОТЫ С ПОДСКАЗКАМИ (HINTS)*************
//******************************************************************
procedure TMTableBrowse.AddFieldHint(FieldTitle, FieldValue, HintText : string; ConditionType : TMConditionType = ctNone);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.AddHint(FieldValue, HintText, ConditionType);
end;

procedure TMTableBrowse.AddFieldImage(FieldTitle, FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;
  BrowseField.AddImage(FieldValue,SelectedImageIndex, DefaultImageIndex, ConditionType);
end;

procedure TMTableBrowse.SetFieldHint(FieldTitle, HintText : string);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.Hint.HintText := HintText;
end;

procedure TMTableBrowse.SetFieldTitleVisible(FieldTitle : string; Visible : boolean = false);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.TitleVisible := Visible;
end;

//******************************************************************
//************МЕТОДА ДЛЯ РАБОТЫ С ИЗОБРАЖЕНИЯМ**********************
//******************************************************************
procedure TMTableBrowse.AddFieldImage(FieldTitle : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.AddImage(SelectedImageIndex, DefaultImageIndex);
end;

procedure TMTableBrowse.AddReferense(FieldTitle : string);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.AddReferense();
end;

procedure TMTableBrowse.AddProgressBar(FieldTitle, CurrentProgressFieldTitle, MaxValueFieldTitle : string);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.AddProgressBar(CurrentProgressFieldTitle, MaxValueFieldTitle);
end;

procedure TMTableBrowse.AddExtendedProgressBar(FieldTitle, CurrentProgressFieldTitle, ExtendedProgressFieldTitle, MaxValueFieldTitle : string);
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.AddExtendedProgressBar(CurrentProgressFieldTitle, ExtendedProgressFieldTitle, MaxValueFieldTitle);
end;

//Возвращает название первого видимого поля
function TMTableBrowse.GetFirstVisibleFieldTitle : string;
var
  i, j,
  MaxOrder,
  FieldsCount : integer;
  BrowseField : TMBrowseField;
  OutBrowseFields : TMBrowseFields;
  flag : boolean;
begin
  Result := '';
  flag := false;
  OutBrowseFields := TMBrowseFields.Create;
  try
    GetAllBrowseFields(OutBrowseFields);

    FieldsCount := OutBrowseFields.GetCount;

    if not UserFieldsOrder
      then
        begin
          for i := 0 to FieldsCount - 1 do
          begin
            BrowseField := OutBrowseFields.GetItem(i);
            if BrowseField.Visible
              then
                begin
                  Result := BrowseField.FieldTitle;
                  break;
                end;
          end
        end
      else
        begin
          MaxOrder := GetMaxFieldOrder;
          for i := 1 to MaxOrder do
          begin
            for j := 0 to FieldsCount - 1 do
            begin
              BrowseField := OutBrowseFields.GetItem(j);

              if not Assigned(BrowseField)
                then break;
              
              if (BrowseField.Visible) and
                 (BrowseField.FieldOrder = i)
                then
                  begin
                    Result := BrowseField.FieldTitle;
                    flag := true;
                    break;
                  end;
            end;
            if flag
              then break;
          end;
        end;
  finally
    OutBrowseFields.DestroyWithoutElementsFree;
  end;
end;

//Возвращает название последнего видимого поля
function TMTableBrowse.GetLastVisibleFieldTitle : string;
var
  i, j,
  MaxOrder,
  FieldsCount : integer;
  BrowseField : TMBrowseField;
  OutBrowseFields : TMBrowseFields;
  flag : boolean;
begin
  Result := '';
  flag := false;
  OutBrowseFields := TMBrowseFields.Create;
  try
    GetAllBrowseFields(OutBrowseFields);

    FieldsCount := OutBrowseFields.GetCount;

    if not UserFieldsOrder
      then
        begin
          for i := FieldsCount - 1 downto 0 do
          begin
            BrowseField := OutBrowseFields.GetItem(i);
            if BrowseField.Visible
              then
                begin
                  Result := BrowseField.FieldTitle;
                  break;
                end;
          end
        end
      else
        begin
          MaxOrder := GetMaxFieldOrder;
          for i := MaxOrder downto 1 do
          begin
            for j := 0 to FieldsCount - 1 do
            begin
              BrowseField := OutBrowseFields.GetItem(j);

              if not Assigned(BrowseField)
                then break;
              
              if (BrowseField.Visible) and
                 (BrowseField.FieldOrder = i)
                then
                  begin
                    Result := BrowseField.FieldTitle;
                    flag := true;
                    break;
                  end;
            end;
            if flag
              then break;
          end;
        end;
  finally
    OutBrowseFields.DestroyWithoutElementsFree;
  end;

end;


function TMTableBrowse.AddBrowseField(FieldName : string; FieldTitle : string; FieldVisible : boolean) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  Result := nil;

  BrowseField := AddBrowseField(FieldName, FieldTitle);

  if not Assigned(BrowseField)
    then Exit;

  BrowseField.Visible := FieldVisible;

  Result := BrowseField;
end;

function TMTableBrowse.AddCalculatedField(QueryText, FieldTitle : string; FieldType : TFieldType) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  BrowseField := TMBrowseField.Create;

  BrowseField.FieldTitle := FieldTitle;
  BrowseField.QueryText := QueryText;
  BrowseField.BrowseFieldType := bftCalculated;
  BrowseField.FieldType := FieldType;

  Result := FBrowseFields.AddItem(BrowseField);
end;


function TMTableBrowse.AddCalculatedFunctionField(QueryText, FieldTitle : string;  FieldType : TFieldType; GetFieldText : TMGetFieldTextFunction) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  BrowseField := TMBrowseField.Create;

  BrowseField.FieldTitle := FieldTitle;
  BrowseField.QueryText := QueryText;
  BrowseField.BrowseFieldType := bftCalculatedFunction;
  BrowseField.FieldType := FieldType;
  BrowseField.GetFieldText := GetFieldText;

  Result := FBrowseFields.AddItem(BrowseField);
end;


function TMTableBrowse.AddDataFunctionField(FieldName : string; FieldTitle : string; GetFieldText : TMGetFieldTextFunction) : TMBrowseField;
var
  BrowseField : TMBrowseField;
  Field : TMField;
begin
  Result := nil;

  Field := FTableStructureRef.GetField(FieldName);

  if not Assigned(Field)
    then Exit;

  BrowseField := TMBrowseField.Create();

  BrowseField.CopyFromField(Field);

  BrowseField.BrowseFieldType := bftDataFunction;
  BrowseField.FieldTitle := FieldTitle;
  BrowseField.GetFieldText := GetFieldText;

  Result := FBrowseFields.AddItem(BrowseField);

end;

function TMTableBrowse.AddAgregateField(QueryText, FieldTitle : string;  FieldType : TFieldType; FieldVisible : boolean = true) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  BrowseField := TMBrowseField.Create;

  BrowseField.FieldTitle := FieldTitle;
  BrowseField.QueryText := QueryText;
  BrowseField.BrowseFieldType := bftAgregate;
  BrowseField.FieldType := FieldType;
  BrowseField.Visible := FieldVisible;

  Result := FBrowseFields.AddItem(BrowseField);
end;

function TMTableBrowse.DeleteBrowseField(FieldTitle : string) : boolean;
begin
  Result := FBrowseFields.DeleteItem(FieldTitle);
end;

function TMTableBrowse.AddGraphicField(FieldTitle : string; FieldType : TFieldType = ftInteger) : TMBrowseField;
var
  BrowseField : TMBrowseField;
begin
  BrowseField := TMBrowseField.Create;

  BrowseField.FieldTitle := FieldTitle;
  BrowseField.BrowseFieldType := bftGraphic;
  BrowseField.FieldType := FieldType;

  Result := FBrowseFields.AddItem(BrowseField);
end;

function TMTableBrowse.GetBrowseFieldsCount() : integer;
begin
  Result := FBrowseFields.GetCount;
end;

function TMTableBrowse.GetBrowseField(FieldTitle : string) : TMBrowseField;
begin
  Result := FBrowseFields.GetItem(FieldTitle);
end;

function TMTableBrowse.GetBrowseField(FieldIndex : integer) : TMBrowseField;
begin
  Result := FBrowseFields.GetItem(FieldIndex);
end;

function TMTableBrowse.AddSortingField(FieldName, TableName : string; RevSorting : boolean = false) : TMBrowseField;
var
  SortingField : TMSortingField;
  Field : TMField;
  TableStructure : TMTableStructure;
begin
  Result := nil;

  TableStructure := ApplicationDataBaseStructure.GetTable(TableName);

  if not Assigned(TableStructure)
    then Exit;

  Field := TableStructure.GetField(FieldName);

  if not Assigned(Field)
    then Exit;

  SortingField := TMSortingField.Create;
  SortingField.CopyFromField(Field);
  //т.к. управление (Get, Add, Delete) BrowseField производится на основании
  //поля FieldTitle то мы задаем значение для этого свойства
  SortingField.FieldTitle := FieldName;
  SortingField.ReverseSorting := RevSorting;


  Result := FSortingFields.AddItem(SortingField);
end;

function TMTableBrowse.AddColorField(FieldName, FieldTitle: string;
  BrowseFieldType: TMBrowseFieldTypes = bftColorText): TMBrowseField;
var
  BrowseField : TMBrowseField;
  Field : TMField;
begin
  Result := nil;

  Field := FTableStructureRef.GetField(FieldName);

  if not Assigned(Field)
    then Exit;

  BrowseField := TMBrowseField.Create();
  BrowseField.CopyFromField(Field);
  
  BrowseField.BrowseFieldType := BrowseFieldType;
  BrowseField.FieldTitle      := FieldTitle;

  Result := FBrowseFields.AddItem(BrowseField);
end;

function TMTableBrowse.GetSortingField(index : integer) : TMSortingField;
begin
  Result := FSortingFields.GetItem(Index);
end;

function TMTableBrowse.GetSortingField(FieldTitle : String) : TMSortingField;
begin
  Result := FSortingFields.GetItem(FieldTitle);
end;

function TMTableBrowse.SetSortingField(FieldTitle : string) : TMSortingField;
begin
  ClearSortingFields;

  Result := AddSortingField(FieldTitle);
end;

function TMTableBrowse.DeleteSortingField(FieldTitle : string) : boolean;
begin
  Result := FSortingFields.DeleteItem(FieldTitle)
end;


function TMTableBrowse.AddSortingField(FieldTitle : string; revSorting : boolean = false) : TMSortingField;
var
  SortingField : TMSortingField;      //который хотим создать
  CurrentBrowseField : TMBrowseField; //который уже прикреплен к таблице
begin
  Result := nil;

  CurrentBrowseField := GetAnyField(FieldTitle);
  if not Assigned(CurrentBrowseField)
    then Exit;

  SortingField := TMSortingField.Create();

  SortingField.CopyFromBrowseField(CurrentBrowseField);
  SortingField.ReverseSorting := revSorting;

  Result := FSortingFields.AddItem(SortingField);
end;

function TMTableBrowse.AddGroupingField(BrowseTitle : string) : TMBrowseField;
var
  NewBrowseField,   //который хотим создать
  CurrentBrowseField : TMBrowseField; //который уже прикреплен к таблице
begin
  Result := nil;

  CurrentBrowseField := FBrowseFields.GetItem(BrowseTitle);
  if not Assigned(CurrentBrowseField)
    then Exit;

  NewBrowseField := TMBrowseField.Create();

  NewBrowseField.CopyFromBrowseField(CurrentBrowseField);

  Result := FGroupingFields.AddItem(NewBrowseField);
end;


/////////////////////////////////////////////////////////////////////////
/////////////////РАБОТА С ПРИСОЕДИНЕНННЫМИ ТАБЛИЦАМИ/////////////////////
/////////////////////////////////////////////////////////////////////////


function TMTableBrowse.AddJoinedTable(TableName : string) : TMTableBrowse;
var
  JoinedTable : TMTableBrowse;
begin
  JoinedTable := TMTableBrowse.Create(TableName);

  JoinedTable.BrowseName := TableName;

  Result := JoinedTables.AddItem(JoinedTable);
end;

function TMTableBrowse.GetJoinedTable(TableName : string) : TMTableBrowse;
begin
  Result := JoinedTables.GetItem(TableName);
end;

function TMTableBrowse.GetJoinedTable(TableIndex : integer) : TMTableBrowse;
begin
  Result := JoinedTables.GetItem(TableIndex);
end;

function TMTableBrowse.GetBrowseFieldsString() : string;
var
  i : integer;
  BrowseFieldsCount : integer;
  BrowseField : TMBrowseField;
begin
  Result := '';

  BrowseFieldsCount := FBrowseFields.GetCount;

  for i := 0 to BrowseFieldsCount - 1 do
  begin
    BrowseField := FBrowseFields.GetItem(i);
    if not Assigned(BrowseField)
      then continue;

    Result := Result + BrowseField.TitleString;

    if i = (BrowseFieldsCount - 1)
      then Result := Result + ' '
      else Result := Result + ', ';
  end;
end;

function TMTableBrowse.GetGroupingFieldsString(): string;
var
  i : integer;
  GroupingFieldsCount : integer;
  BrowseFieldsCount : integer;
  BrowseField : TMBrowseField;

  BrowseFields : TMBrowseFields;
begin
  Result := '';

  GroupingFieldsCount := FGroupingFields.GetCount;

  if GroupingFieldsCount = 0
    then Exit;
  

  for i := 0 to GroupingFieldsCount - 1 do
  begin
    BrowseField := FGroupingFields.GetItem(i);

    if not Assigned(BrowseField)
      then Continue;

    //В программе предусмотрены два вида отображения
    //сортировочный полей: по псевдониму, указанному после "AS"
    //либо по имени таблицы и s. Если Имя, указанное после "AS"
    //совпадает с именем поля в таблице, то мы считаем, что поле второго типа
    if BrowseField.FieldTitle = BrowseField.FieldName
      then Result := Result + BrowseField.TableName + '.' + BrowseField.FieldName
      else Result := Result + '"' + BrowseField.FieldTitle + '"';

    if i = (GroupingFieldsCount - 1)
      then Result := Result + ' '
      else Result := Result + ', ';
  end;

  if (GroupingFieldsCount > 0)
    then Result := ' GROUP BY ' + Result;

  //Если мы работаем с FireBird, то в поле GroupBy должны находиться
  //все столбцы не включенные в агрегатные функции
  if GLOBAL_DATABASE_TYPE <> dtFireBird
    then Exit;

  BrowseFieldsCount := FBrowseFields.GetCount;

  BrowseFields := TMBrowseFields.Create;
  try
    //В начале записываем в BrowseElements те поля, которые соответсвуют условию отбора
    for i := 0 to BrowseFieldsCount - 1 do
    begin
      BrowseField := GetBrowseField(i);

      if not Assigned(BrowseField)
        then Continue;

      if (BrowseField.BrowseFieldType <> bftGraphic) and
         (BrowseField.BrowseFieldType <> bftAgregate) and
         (not Assigned(FGroupingFields.GetItem(BrowseField.FieldTitle)))
        then BrowseFields.AddItem(BrowseField);
    end;

    //Добавляем соответствующие поля отбора в условия группировки
    BrowseFieldsCount := BrowseFields.GetCount;

    for i := 0 to BrowseFieldsCount - 1 do
    begin
      BrowseField := GetBrowseField(i);

      if not Assigned(BrowseField)
        then Continue;

      Result := Result + '"' + BrowseField.FieldTitle + '"';

      if i < BrowseFieldsCount
        then Result := Result + ', '
        else Result := Result + ' ';
    end;
  finally
    BrowseFields.DestroyWithoutElementsFree;
  end;
end;


function TMTableBrowse.GetSortingFieldsString() : string;
var
  i : integer;
  SortingFieldsCount : integer;
  SortingField : TMSortingField;
  last : boolean;
  tmpSortingFields : TMSortingFields;
begin
  Result := '';

  tmpSortingFields := TMSortingFields.Create;
  try
    tmpSortingFields.AddItems(FSortingFields);
    SortingFieldsCount := FSortingFields.GetCount;

    //Если поле у нас является прикрепленным, то первым параметром сортировки
    //должно быть именно оно
    SortingField := tmpSortingFields.GetItem(FPickedUpFieldTitle);
    if Assigned(SortingField)
      then
        begin
          if SortingFieldsCount > 1
            then last := false
            else last := true;

          Result := SortingField.GetSortingFieldString(last);
          tmpSortingFields.DropItem(FPickedUpFieldTitle);
          SortingFieldsCount := tmpSortingFields.GetCount;
        end;

    for i := 0 to SortingFieldsCount - 1 do
      begin
        SortingField := tmpSortingFields.GetItem(i);

        if not Assigned(SortingField)
          then Continue;

        if (i = (SortingFieldsCount - 1))
          then last := True
          else last := False;

        Result := Result + SortingField.GetSortingFieldString(last)
      end;
  finally
    tmpSortingFields.DestroyWithoutElementsFree;
  end;

end;

function TMTableBrowse.GetJoinedFieldsString() : string;
var
  i, j : integer;
  JoinedTablesCount : integer;
  JoinedFieldsCount : integer;

  JoinedTable : TMTableBrowse;
  JoinedField : TMBrowseField;
begin
  Result := '';

  JoinedTablesCount := JoinedTables.GetCount;

  if JoinedTablesCount = 0
    then Exit;

  for i := 0 to JoinedTablesCount - 1 do
  begin
    JoinedTable := GetJoinedTable(i);

    if not Assigned(JoinedTable)
      then Continue;

    JoinedFieldsCount := JoinedTable.GetBrowseFieldsCount;
    //После списка GetBrowseFieldsString нет запятой, поэтому добавляем
    //её только в том случае, если в текущей JoinedTable есть поля
    if JoinedFieldsCount > 0
      then Result := Result + ', ';
    for j := 0 to JoinedFieldsCount - 1 do
    begin
      JoinedField := JoinedTable.GetBrowseField(j);

      if not Assigned(JoinedField)
        then Continue;

      Result := Result + JoinedField.TitleString;

      if j = (JoinedFieldsCount - 1)
        then Result := Result + ' '
        else Result := Result + ', ';
    end;

    Result := Result + JoinedTable.GetJoinedFieldsString; //РЕКУРСИЯ!!!!
  end;

end;

function TMTableBrowse.GetJoinedTablesString() : string;
var
  i,
  JoinedTablesCount : integer;

  JoinedTable : TMTableBrowse;

  MainTableStructure,  //в которой находится внешний ключ
  DependTableStructure : TMTableStructure; //на которую ссылается главная

  FKFieldMainTableString,
  PKFieldDependTableString: string;
begin
  Result := '';

  JoinedTablesCount := JoinedTables.GetCount;

  if JoinedTablesCount = 0
    then Exit;

  for i := 0 to JoinedTablesCount - 1 do
  begin
    JoinedTable := GetJoinedTable(i);
    if not Assigned(JoinedTable)
      then Continue;

    //необходимо определить, какая из таблиц
    //является главной, а какая зависимой

    if Assigned(FTableStructureRef.GetForeignKey(JoinedTable.FTableName))
      then
        begin
          MainTableStructure := FTableStructureRef;
          DependTableStructure := JoinedTable.FTableStructureRef;
        end
      else
        begin
          MainTableStructure := JoinedTable.FTableStructureRef;
          DependTableStructure := FTableStructureRef;
        end;

    FKFieldMainTableString := MainTableStructure.GetForeignKey(DependTableStructure.TableName).FieldString;
    PKFieldDependTableString := DependTableStructure.GetPKField.FieldString;

    Result := Result + ' LEFT JOIN ' + JoinedTable.FTableName + ' ON (' +
              FKFieldMainTableString + ' = ' + PKFieldDependTableString + ')';
    //!!!!!!!!!!!!!!!!!!!!!РЕКУРСИЯ!!!!!!!!!!!!!!!!!!!!!
    Result := Result + JoinedTable.GetJoinedTablesString;
  end;
end;


function TMTableBrowse.GetUserOrderBrowseFieldsString() : string;
var
  OutBrowseFields : TMBrowseFields;

  BrowseField : TMBrowseField;
  OutBrowseFieldsCount,
  i, j,
  MaxFieldOrder : integer;
  flag : boolean;
begin
  //Выводит только те поля, в которых FieldOrder был указан > 1
  Result := '';

  OutBrowseFields := TMBrowseFields.Create;
  try
    OutBrowseFields.AddItems(FBrowseFields);
    GetJoinedBrowseFields(OutBrowseFields);

    MaxFieldOrder := OutBrowseFields.MaxFieldOrder;

    for i := 1 to MaxFieldOrder do
    begin
      OutBrowseFieldsCount := OutBrowseFields.GetCount;
      flag := false;
      for j := 0 to OutBrowseFieldsCount - 1 do
      begin
        BrowseField := OutBrowseFields.GetItem(j);

        if not Assigned(BrowseField)
          then Continue;

        if (BrowseField.FieldOrder = i)
          then
            begin
              Result := Result + BrowseField.TitleString;
              //Убираем уже отображенный элемент из списка
              OutBrowseFields.DropItem(BrowseField);
              flag := true;
              break;
            end;
      end;

      if i < MaxFieldOrder
        then
          begin
            if flag
              then Result := Result + ', '
          end
        else Result := Result + ' ';
    end;
  finally
    //вызываем деструктор, который не уничтожает Items
    OutBrowseFields.DestroyWithoutElementsFree;
  end;
end;

function TMTableBrowse.GetWhereString() : string;
var
  DefaultWhereClause,
  UserWhereClause : string;
begin
  Result := '';

  DefaultWhereClause := DefaultQueryConstructor.GetWhereClause();

  if DefaultWhereClause = ''
    then Result := UserQueryConstructor.GetWhereClause
    else
      begin
        UserWhereClause := UserQueryConstructor.GetWhereClause(False);
        if UserWhereClause = ''
          then Result := DefaultWhereClause
          else Result := 'WHERE (' +
                         DefaultQueryConstructor.GetWhereClause(false) +
                         ') AND (' +
                         UserWhereClause + ')';

      end;
end;

function TMTableBrowse.GetAfterFromString : string;
var
  GroupingClause : string; //GROUP BY FieldName1, ..., FieldNameN
  SortingClause : string; //ORDER BY FieldName1, ..., FieldNameN DESC/ASC
  JoinedTablesClause : string; //LEFT JOIN TABLENAME ON (A = B)
  WhereClause : string; //Строка с условиями where
begin
  Result := '';

  GroupingClause     := GetGroupingFieldsString;
  SortingClause      := GetSortingFieldsString;
  JoinedTablesClause := GetJoinedTablesString;
  WhereClause        := GetWhereString;

  Result := JoinedTablesClause + ' ' +
            WhereClause +
            GroupingClause +
            ' ORDER BY ' + SortingClause;
end;

 //******************************************************************
//************ МЕТОДЫ, ДЛЯ РАБОТЫ С УСЛОВИЯМИ************************
//*******************************************************************
function TMTableBrowse.AddCondition(ChoosenQueryConstructor : TMQueryConstructor;
                                    FieldTitle : string;
                                    ConditionType : TMConditionType;
                                    FieldValue : variant) : TMCondition;
var
  BrowseField : TMBrowseField;
begin
  Result := nil;
  BrowseField := GetBrowseField(FieldTitle);
  if not Assigned(BrowseField)
    then Exit;

  Result := ChoosenQueryConstructor.AddCondition(BrowseField,
                                                 ConditionType,
                                                 FieldValue);
end;

function TMTableBrowse.AddCondition(ChoosenQueryConstructor : TMQueryConstructor;
                                    FieldName,
                                    TableName : string;
                                    ConditionType : TMConditionType;
                                    FieldValue : variant) : TMCondition;
begin
  Result := ChoosenQueryConstructor.
            AddCondition(TableName,
                         FieldName,
                         ConditionType,
                         FieldValue);
end;

//////////////TMTableBrowsesSSSSSSS//////////////////////////////////
constructor TMTableBrowses.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMTableBrowses.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;

procedure TMTableBrowses.CopyFromTableBrowses(TableBrowses : TMTableBrowses);
var
  i, TableBrowsesCount : integer;
  NewTableBrowse, OldTableBrowse : TMTableBrowse;
begin
  if not Assigned(TableBrowses)
    then Exit;

  TableBrowsesCount := TableBrowses.GetCount;
  for i := 0 to TableBrowsesCount - 1 do
  begin
    OldTableBrowse := TableBrowses.GetItem(i);

    if not Assigned(OldTableBrowse)
      then break;

    NewTableBrowse := TMTableBrowse.Create('');
    NewTableBrowse.CopyFromTableBrowse(OldTableBrowse);
    AddItem(NewTableBrowse);
  end;
end;

function TMTableBrowses.GetItem(ItemIndex : integer) : TMTableBrowse;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMTableBrowse;
end;


function TMTableBrowses.GetItem(ItemTitle : string) : TMTableBrowse;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMTableBrowses.AddItem(Item : TMTableBrowse) : TMTableBrowse;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.BrowseName, Item) >= 0
    then Result := Item;
end;

function TMTableBrowses.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


function TMTableBrowses.DeleteItem(ItemIndex : integer) : boolean;
var
  RecordField : TMTableBrowse;
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

procedure TMTableBrowses.Reset;
var
  i : integer;
  RecordField : TMTableBrowse;
begin
  for i := 0 to GetCount - 1 do
  begin
    RecordField := Items.Objects[i] as TMTableBrowse;
    if Assigned(RecordField)
      then RecordField.Free;
  end;

  Items.Clear;
end;

function TMTableBrowses.GetCount : integer;
begin
  Result := Items.Count;
end;


end.
