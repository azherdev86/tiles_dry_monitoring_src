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
    FBrowseFields  : TMBrowseFields;  //������ �����, ������� ����� ���������� � �������
    FSortingFields : TMSortingFields;  //������ �����, �� ������� �������������� ����������
    FGroupingFields   : TMBrowseFields;  //������ �����, �� ������� �������������� �����������
    FTableName : string;              //��� �������
    FUserFieldsOrder : boolean;       //������ ��������� ������� ���������� �������� � ������� (��/���)

    //������, ������� � ������� �� �����
    FTableStructureRef : TMTableStructure;  //������ �� ��������� ��������������� �������
    JoinedTables : TMTableBrowses;          //������ �������������� ������

    FPickedUpFieldTitle : string; //����, �� �������� �������������� ����������� ������
    //���������� UNION. ���� ��������� - ��� PickUpedField = 0, � ������ ��� PickUpedField = 1
    FCheckFieldTitle : string; //�����, ������� ������������ � �������� CheckBox = 0 �� �������, 1 - �������
    procedure Reset;
    procedure GetJoinedBrowseFields(var OutBrowseFields : TMBrowseFields); //!!!�����������!!!

 //   procedure GetJoinedBrowseFields(JoinedBrowseFields :
  public
    BrowseName : string;
    DefaultQueryConstructor,  //�������� � ������
    UserQueryConstructor : TMQueryConstructor;
    //����������� �������
    procedure CopyFromTableBrowse(TableBrowse : TMTableBrowse);
    procedure GetAllBrowseFields(var OutBrowseFields : TMBrowseFields);

    //******************************************************************
    //************ �������� ����� ������********************************
    //******************************************************************
    function GetSelectQuery : string; //������ Select �� ����� �����������
    //******************************************************************

    //******************************************************************
    //*************������ ���� ���������� �������� ������������� ����***
    //******************************************************************
    function GetDistinctValuesQuery(FieldTitle : string) : string; overload;//����� ��� �������� ��������
    function GetDistinctValuesQuery(BrowseField : TMBrowseField) : string; overload;//����� ��� �������� ��������

    //******************************************************************
    //************ ������ � ������, ������������� � ��������************
    //******************************************************************

    //���������� ����� ��� ����������� � TableBrowse

    function AddColorField(FieldName, FieldTitle: string;
      BrowseFieldType: TMBrowseFieldTypes = bftColorText): TMBrowseField;

    function AddBrowseField(FieldName: string; FieldTitle : string;
      BrowseFieldType: TMBrowseFieldTypes = bftData): TMBrowseField; overload;

    function AddBrowseField(FieldName: string; FieldTitle: string;
      FieldVisible: boolean): TMBrowseField; overload;

    function AddDataFunctionField(FieldName: string; FieldTitle: string;
      GetFieldText: TMGetFieldTextFunction) : TMBrowseField;

    //���������� ����, ������� � ���� �������� SQL Text
    function AddCalculatedField(QueryText, FieldTitle: string;
      FieldType: TFieldType) : TMBrowseField;

    //���������� ����, ������� � ���� �������� SQL Text
    function AddCalculatedFunctionField(QueryText, FieldTitle: string;
      FieldType: TFieldType; GetFieldText: TMGetFieldTextFunction): TMBrowseField;

    function AddGraphicField(FieldTitle: string;
      FieldType: TFieldType = ftInteger): TMBrowseField;

    function AddAgregateField(QueryText, FieldTitle: string;
      FieldType: TFieldType; FieldVisible: boolean = true): TMBrowseField;

    function DeleteBrowseField(FieldTitle: string): boolean;

    //���������� ����� ��� ���������
    function GetBrowseFieldsCount() : integer;

    //������ � � ����� ��������� �� ����� � �� �������
    function GetBrowseField(FieldTitle : string) : TMBrowseField; overload;

    function GetBrowseField(FieldIndex : integer) : TMBrowseField; overload;

    //******************************************************************
    //************ ������ � ��������������� ���������*******************
    //******************************************************************

    //���������� �������������� ������
    function AddJoinedTable(TableName : string) : TMTableBrowse;
    //������ � ��������������  �������� �� ����� � �� �������
    function GetJoinedTable(TableName : string) : TMTableBrowse; overload;
    function GetJoinedTable(TableIndex : integer) : TMTableBrowse; overload;

    //******************************************************************
    //************ ������ � ������ ��� ����������***********************
    //******************************************************************
    //���������� ����� ����������
    function AddSortingField(FieldTitle : string; revSorting : boolean = false) : TMSortingField; overload;
    //������������� ���� ������� ��������� �������� ���������� ���� �� ��� �����,
    //������� ����������� � ������ ������������� FBrowseFields
    function AddSortingField(FieldName, TableName : string; RevSorting : boolean = false) : TMBrowseField; overload;
    function GetSortingField(Index : integer) : TMSortingField;    overload;
    function GetSortingField(FieldTitle : string) : TMSortingField; overload;
    function SetSortingField(FieldTitle : string) : TMSortingField;
    function DeleteSortingField(FieldTitle : string) : boolean;
    procedure ClearSortingFields;
    function GetSortingFieldsCount() : integer;

    //******************************************************************
    //************ ������ � ������ ��� �����������***********************
    //******************************************************************
    //���������� ����� ����������
    function AddGroupingField(BrowseTitle : string) : TMBrowseField;
    procedure ClearGroupingFields;

    //******************************************************************
    //************ ������, ��� ������ � ���������***********************
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
    //************ ������,��� ������ � ������ ��� ������������**********
    //******************************************************************
    function SetPickUpedField(FieldTitle : string) : TMBrowseField;

    procedure DeletePickUpedField();

    //******************************************************************
    //************ ������,��� ������ � CHECKBOX ������******************
    //******************************************************************
    function SetCheckField(FieldTitle : string) : TMBrowseField;
    procedure DeleteCheckField();


    //******************************************************************
    //************ ������, ����� ��� ���� �����*************************
    //******************************************************************
    //���������� ���� BrowseField �� FBrowseFields, � ����� �� ����� ��
    //�������������� ������ (JoinedTables)
    function GetAnyField(FieldTitle : string) : TMBrowseField;
    function GetJoinedField(FieldTitle : string) : TMBrowseField;  //!!!��������!!!!
    //��������� ������� ���������� ��� ����
    procedure SetFieldOrder(BrowseTitle : string; FieldOrder : integer);
    function GetMaxFieldOrder() : integer;
    //��������� ������ ������� � �������
    procedure SetFieldWidth(BrowseTitle : string; FieldWidth : integer = 0);

    //******************************************************************
    //************ ������, ��� ������ � ��������� �� ���� �����*********
    //******************************************************************
    procedure SetAllFieldFilter(FilterValue : string);

    //******************************************************************
    //************ ������, ��� ������ � ����������� (HINTS)*************
    //******************************************************************
    procedure AddFieldHint(FieldTitle, FieldValue, HintText : string; ConditionType : TMConditionType = ctNone);
    procedure SetFieldHint(FieldTitle, HintText : string);

    //******************************************************************
    //************ ������, ��� ������ � ����������� (HINTS)*************
    //******************************************************************
    procedure SetFieldTitleVisible(FieldTitle : string; Visible : boolean = false);


    //******************************************************************
    //************������ ��� ������ � ������������**********************
    //******************************************************************
    procedure AddFieldImage(FieldTitle : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1); overload;
    procedure AddFieldImage(FieldTitle, FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone); overload;
    procedure AddReferense(FieldTitle : string);
    procedure AddProgressBar(FieldTitle, CurrentProgressFieldTitle, MaxValueFieldTitle : string);
    procedure AddExtendedProgressBar(FieldTitle, CurrentProgressFieldTitle, ExtendedProgressFieldTitle, MaxValueFieldTitle : string);

    //******************************************************************
    //************������ � ������� � ���������� ������� ����� **********
    //******************************************************************
    //���������� �������� ������� �������� ����
    function GetFirstVisibleFieldTitle : string;

    //���������� �������� ���������� �������� ����
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
    //************ ������, ��� ������������ ������� SELECT**************
    //******************************************************************
    //������ �����, ������ ����� ����� SELECT � �� ����� FROM
    function GetBrowseFieldsString()     : string;
    //������ ����� ����������. ������� � ����� �������. ���� ����� ��� ����������
    //���� �� �������, ���������� ����������� �� ���������� ����� �������
    function GetSortingFieldsString()    : string;
    //������ �����, �� ������� ����������� ����������� (���� ������ ��������� � ������ Browse ����� )
    function GetGroupingFieldsString()    : string;
    //������ ����� �� �������������� ������ ����� ����� SELECT � �� ����� FROM
    function GetJoinedFieldsString() : string;  //!!! ����������� !!!
    //������ ������������� ������. ������� ����� ����� FROM TableName
    function GetJoinedTablesString() : string;  //!!! ����������� !!!
    //������ �����, ������ ����� ����� SELECT � �� ����� FORM � ������ ������� ����������
    function GetUserOrderBrowseFieldsString() : string;
    //������ ������� �������
    function GetWhereString() : string;
    //����� �������, ������� ���������� ����� ���� FROM (�������� � ����)
    //GroupingFields, JoinedTables
    function GetAfterFromString : string; virtual;
     //******************************************************************
    //************ ������, ��� ������ � ���������************************
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

  //� ����������� ��������� �� �����
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

  //������, ������� � ������� �� �����
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


procedure TMTableBrowse.GetJoinedBrowseFields(var OutBrowseFields : TMBrowseFields); //!!!�����������!!!
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

    JoinedTable.GetJoinedBrowseFields(OutBrowseFields);  //!!!��������!!!
  end;
end;

procedure TMTableBrowse.GetAllBrowseFields(var OutBrowseFields : TMBrowseFields);
begin
  if not Assigned(OutBrowseFields)
    then Exit;

  OutBrowseFields.AddItems(FBrowseFields);
  GetJoinedBrowseFields(OutBrowseFields);
end;


function TMTableBrowse.GetSelectQuery : string; //������ Select
var
  BrowseFieldsClause : string; // FieldName1 as "FieldTitle1", ..., FieldNameN as "FieldTitleN"
  JoinedFieldsClause : string;
  AfterFromClause : string;
begin
  Result := '';

  //���� ���������� ���������������� ������� ���������� �����
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
    //************ ������,��� ������ � ������ ��� UNION*****************
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


function TMTableBrowse.GetDistinctValuesQuery(FieldTitle : string) : string; //����� ��� �������� ��������
var
  BrowseField : TMBrowseField;
begin
  BrowseField := GetAnyField(FieldTitle);

  Result := GetDistinctValuesQuery(BrowseField);
end;

function TMTableBrowse.GetDistinctValuesQuery(BrowseField : TMBrowseField) : string; //����� ��� �������� ��������
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

  //���� ���� �� ��������� � FBrowseField ���� ��� � �������������� ��������
    Result := GetJoinedField(FieldTitle);
end;

function TMTableBrowse.GetJoinedField(FieldTitle : string) : TMBrowseField; //!!!�����������!!!
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
      else Result := JoinedTable.GetJoinedField(FieldTitle); //!!!��������!!!
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
//************ ������, ��� ������ � ��������� �� ���� �����*********
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
//************ ������, ��� ������ � ����������� (HINTS)*************
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
//************������ ��� ������ � ������������**********************
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

//���������� �������� ������� �������� ����
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

//���������� �������� ���������� �������� ����
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
  //�.�. ���������� (Get, Add, Delete) BrowseField ������������ �� ���������
  //���� FieldTitle �� �� ������ �������� ��� ����� ��������
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
  SortingField : TMSortingField;      //������� ����� �������
  CurrentBrowseField : TMBrowseField; //������� ��� ���������� � �������
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
  NewBrowseField,   //������� ����� �������
  CurrentBrowseField : TMBrowseField; //������� ��� ���������� � �������
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
/////////////////������ � ���������������� ���������/////////////////////
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

    //� ��������� ������������� ��� ���� �����������
    //������������� �����: �� ����������, ���������� ����� "AS"
    //���� �� ����� ������� � s. ���� ���, ��������� ����� "AS"
    //��������� � ������ ���� � �������, �� �� �������, ��� ���� ������� ����
    if BrowseField.FieldTitle = BrowseField.FieldName
      then Result := Result + BrowseField.TableName + '.' + BrowseField.FieldName
      else Result := Result + '"' + BrowseField.FieldTitle + '"';

    if i = (GroupingFieldsCount - 1)
      then Result := Result + ' '
      else Result := Result + ', ';
  end;

  if (GroupingFieldsCount > 0)
    then Result := ' GROUP BY ' + Result;

  //���� �� �������� � FireBird, �� � ���� GroupBy ������ ����������
  //��� ������� �� ���������� � ���������� �������
  if GLOBAL_DATABASE_TYPE <> dtFireBird
    then Exit;

  BrowseFieldsCount := FBrowseFields.GetCount;

  BrowseFields := TMBrowseFields.Create;
  try
    //� ������ ���������� � BrowseElements �� ����, ������� ������������ ������� ������
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

    //��������� ��������������� ���� ������ � ������� �����������
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

    //���� ���� � ��� �������� �������������, �� ������ ���������� ����������
    //������ ���� ������ ���
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
    //����� ������ GetBrowseFieldsString ��� �������, ������� ���������
    //� ������ � ��� ������, ���� � ������� JoinedTable ���� ����
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

    Result := Result + JoinedTable.GetJoinedFieldsString; //��������!!!!
  end;

end;

function TMTableBrowse.GetJoinedTablesString() : string;
var
  i,
  JoinedTablesCount : integer;

  JoinedTable : TMTableBrowse;

  MainTableStructure,  //� ������� ��������� ������� ����
  DependTableStructure : TMTableStructure; //�� ������� ��������� �������

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

    //���������� ����������, ����� �� ������
    //�������� �������, � ����� ���������

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
    //!!!!!!!!!!!!!!!!!!!!!��������!!!!!!!!!!!!!!!!!!!!!
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
  //������� ������ �� ����, � ������� FieldOrder ��� ������ > 1
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
              //������� ��� ������������ ������� �� ������
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
    //�������� ����������, ������� �� ���������� Items
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
  WhereClause : string; //������ � ��������� where
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
//************ ������, ��� ������ � ���������************************
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
