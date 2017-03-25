unit CConditionGroups;

interface

uses CConditions,
     DB,
     CBrowseFields,
     CRecordFields,
     CFields,
     Classes;

//Группы условий, для запросов с несколькими условиями
type
  TMConditionGroup = class
    constructor Create;
    destructor Destroy; override;
  public
    OperationIn : TMOperationType;
    //Операция внутри группы, например ((ID = 2) AND (ID <> 4) AND (ID = 6)) OR (следующая группа)
    //Операция внутри группы будет AND
    OperationAfter : TMOperationType;
    //Операция после группы. В примере - OR
    function GetCondition(ConditionIndex : integer) : TMCondition; overload;

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
    procedure CopyFromConditionGroup(ConditionGroup : TMConditionGroup);

  private
    Items : TStringList;
    FCounter : integer;
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

//Группы условий. Оперируют объектами TMConditionGroup
type
  TMConditionGroups = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMConditionGroup; overload;
    function GetItem(ItemName : string) : TMConditionGroup; overload;

    function AddItem(Item : TMConditionGroup) : TMConditionGroup;
    function DeleteItem(ItemName : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;
    procedure CopyFromConditionGroups(ConditionGroups : TMConditionGroups);

  private
    Items : TStringList;
    FCounter : integer;

  end;


implementation

uses SysUtils,
     CTableStructure,
     LApplicationGlobals;

//*************************************************************************
//TMConditionGroup  - группы условий

constructor TMConditionGroup.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMConditionGroup.Destroy;
begin
  Reset;
  Items.Free;
  inherited;
end;

function TMConditionGroup.GetCondition(ConditionIndex : integer) : TMCondition;
begin
  Result := nil;

  if ConditionIndex < 0
    then Exit;

  Result := Items.Objects[ConditionIndex] as TMCondition;
end;


procedure TMConditionGroup.Reset;
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
  OperationIn    := otNone;
  OperationAfter := otNone;
  Fcounter := 0;
end;

function TMConditionGroup.GetConditionsCount : integer;
begin
  Result := Items.Count;
end;

procedure TMConditionGroup.CopyFromConditionGroup(ConditionGroup : TMConditionGroup);
var
  i : integer;
  ConditionsCount : integer;
  NewCondition, OldCondition : TMCondition;
begin
  OperationIn    := ConditionGroup.OperationIn;
  OperationAfter := ConditionGroup.OperationAfter;

  ConditionsCount := ConditionGroup.GetConditionsCount;
  for i := 0 to ConditionsCount - 1 do
  begin
    OldCondition := ConditionGroup.GetCondition(i);
    if not Assigned(OldCondition)
      then Break;

    NewCondition := TMCondition.Create;
    NewCondition.CopyFromCondtion(OldCondition);
    AddCondition(NewCondition);
  end;
end;

function TMConditionGroup.AddCondition(Condition : TMCondition) : TMCondition;
begin
  Result := nil;

  if not Assigned(Condition)
    then Exit;

  if Items.AddObject(IntToStr(FCounter), Condition) >= 0
    then Result := Condition;

  INC(FCounter);
end;


function TMConditionGroup.AddCondition(FieldName : string;
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

function TMConditionGroup.AddCondition(RecordField : TMRecordField;
                                         ConditionType : TMConditionType;
                                         FieldValue : variant) : TMCondition;
begin
  Result := nil;

  if not Assigned(RecordField)
    then Exit;

  Result := AddCondition(RecordField.FieldName, '', '', ConditionType, FieldValue, RecordField.FieldType, RecordField.TableName);
end;

function TMConditionGroup.AddCondition(Field : TMField;
                                         ConditionType : TMConditionType;
                                         FieldValue : variant) : TMCondition;
begin
  Result := nil;

  if not Assigned(Field)
    then Exit;

  Result := AddCondition(Field.FieldName, '', '', ConditionType, FieldValue, Field.FieldType, Field.TableName);
end;

function TMConditionGroup.AddCondition(BrowseField : TMBrowseField;
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

function TMConditionGroup.AddCondition(TableName, FieldName : string;
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


//***************************************************************************
//TMConditionGroups - список групп условий
constructor TMConditionGroups.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMConditionGroups.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMConditionGroups.GetItem(ItemIndex : integer) : TMConditionGroup;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMConditionGroup;
end;


function TMConditionGroups.GetItem(ItemName : string) : TMConditionGroup;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemName);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMConditionGroups.AddItem(Item : TMConditionGroup) : TMConditionGroup;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(IntToStr(FCounter), Item) >= 0
    then Result := Item;
  INC(FCounter);
end;

function TMConditionGroups.DeleteItem(ItemName : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemName);

  Result := DeleteItem(ItemIndex);
end;


function TMConditionGroups.DeleteItem(ItemIndex : integer) : boolean;
var
  ConditionGroup : TMConditionGroup;
begin
  Result := FALSE;
  ConditionGroup := GetItem(ItemIndex);
  if Assigned(ConditionGroup) then
    begin
      Items.Delete(ItemIndex);
      ConditionGroup.Free;
      Result := TRUE;
    end;
end;

procedure TMConditionGroups.Reset;
var
  i, count : integer;
  ConditionGroup : TMConditionGroup;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    ConditionGroup := Items.Objects[i] as TMConditionGroup;
    if Assigned(ConditionGroup)
      then ConditionGroup.Free;
  end;

  Items.Clear;
  FCounter := 0;
end;

function TMConditionGroups.GetCount : integer;
begin
  Result := Items.Count;
end;

procedure TMConditionGroups.CopyFromConditionGroups(ConditionGroups : TMConditionGroups);
var
  i,
  GroupsCount : integer;

  OldConditionGroup : TMConditionGroup;
  NewConditionGroup : TMConditionGroup;
begin
  if not Assigned(ConditionGroups)
    then Exit;

  GroupsCount := ConditionGroups.GetCount;

  for i := 0 to GroupsCount - 1 do
  begin
    OldConditionGroup := ConditionGroups.GetItem(i);
    if not Assigned(OldConditionGroup)
      then Exit;

    NewConditionGroup := TMConditionGroup.Create;
    NewConditionGroup.CopyFromConditionGroup(OldConditionGroup);
    AddItem(NewConditionGroup);
  end;
end;

end.
