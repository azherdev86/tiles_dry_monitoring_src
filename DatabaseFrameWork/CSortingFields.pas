unit CSortingFields;

interface

uses CBrowseFields,
     Classes;


type
  TMSortingField = class (TMBrowseField)
    public
      ReverseSorting : boolean;
      procedure CopyFromSortingField(SortingField : TMSortingField);
      procedure Reset; override;
      function LoadFromBrowseField(BrowseField : TMBrowseField; RevSorting : boolean = false) : TMSortingField;
      function GetSortingFieldString(last : boolean = false) : string;
  end;

type
  TMSortingFields = class
    constructor Create;
    destructor Destroy; override;
    destructor DestroyWithoutElementsFree;  //деструктор без удаления Items

  public

    procedure CopyFromSortingFields(SortingFields : TMSortingFields);
    function GetItem(ItemIndex : integer) : TMSortingField; overload;
    function GetItem(ItemTitle : string) : TMSortingField; overload;

    function AddItem(Item : TMSortingField) : TMSortingField;
    function AddItems(SortingFields : TMSortingFields) : integer;

    function DeleteItem(ItemTitle : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function DropItem(ItemIndex : integer) : boolean; overload;
    function DropItem(ItemTitle : string) : boolean; overload;
    function DropItem(Item : TMBrowseField) : boolean; overload;


    function GetCount : integer;
    procedure Reset;


  private
    Items : TStringList;
  private
    procedure ResetWithoutElementsFree;

  end;


implementation

procedure TMSortingField.Reset;
begin
  inherited Reset;
  ReverseSorting := False;
end;

procedure TMSortingField.CopyFromSortingField(SortingField : TMSortingField);
begin
  if not Assigned(SortingField)
    then Exit;

  ReverseSorting := SortingField.ReverseSorting;

  CopyFromBrowseField(SortingField as TMBrowseField);  
end;

function TMSortingField.LoadFromBrowseField(BrowseField : TMBrowseField; RevSorting : boolean = false) : TMSortingField;
begin
  CopyFromBrowseField(BrowseField);
  ReverseSorting := RevSorting;
  Result := Self;
end;


function TMSortingField.GetSortingFieldString(last : boolean = false) : string;
begin
  if FieldTitle = FieldName
      then Result := Result + TableName + '.' + FieldName
      else Result := Result + '"' + FieldTitle + '"';

  //В любом случае дописываем направление сортировки
  if ReverseSorting
    then Result := Result + ' DESC'
    else Result := Result + ' ASC';

  if last       
    then Result := Result + ' '
    else Result := Result + ', ';
end;

//////////////TMBrowseFieldsSSSSSSS//////////////////////////////////
constructor TMSortingFields.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMSortingFields.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;

destructor TMSortingFields.DestroyWithoutElementsFree;
begin
  ResetWithoutElementsFree;
  Items.Free;
  inherited Destroy;
end;

procedure TMSortingFields.CopyFromSortingFields(SortingFields : TMSortingFields);
var
  i, SortingFieldsCount : integer;
  NewSortingField, OldSortingField : TMSortingField;
begin
  if not Assigned(SortingFields)
    then Exit;

  SortingFieldsCount := SortingFields.GetCount;

  for i := 0 to SortingFieldsCount - 1 do
  begin
    OldSortingField := SortingFields.GetItem(i);

    if not Assigned(OldSortingField)
      then Exit;

    NewSortingField := TMSortingField.Create;
    NewSortingField.CopyFromSortingField(OldSortingField);
    AddItem(NewSortingField);
  end;
    
  


end;

function TMSortingFields.GetItem(ItemIndex : integer) : TMSortingField;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMSortingField;
end;

function TMSortingFields.AddItems(SortingFields : TMSortingFields) : integer;
var
  i,
  SortingFieldsCount : integer;
  SortingField : TMSortingField;
begin
  SortingFieldsCount := SortingFields.GetCount;
  Result := 0;

  for i := 0 to SortingFieldsCount - 1 do
  begin
    SortingField := SortingFields.GetItem(i);

    if not Assigned(SortingField)
      then Continue;

    if Assigned(AddItem(SortingField))
      then INC(Result);
  end;
    
end;


function TMSortingFields.GetItem(ItemTitle : string) : TMSortingField;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMSortingFields.AddItem(Item : TMSortingField) : TMSortingField;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.FieldTitle, Item) >= 0
    then Result := Item;
end;


function TMSortingFields.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


function TMSortingFields.DeleteItem(ItemIndex : integer) : boolean;
var
  RecordField : TMSortingField;
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

function TMSortingFields.DropItem(ItemIndex : integer) : boolean;
begin
  result := true;
  Items.Delete(ItemIndex);
end;

function TMSortingFields.DropItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DropItem(ItemIndex);
end;


function TMSortingFields.DropItem(Item : TMBrowseField) : boolean;
begin
  Result := DropItem(Item.FieldTitle);
end;


procedure TMSortingFields.Reset;
var
  i, count: integer;
  RecordField : TMSortingField;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    RecordField := Items.Objects[i] as TMSortingField;
    if Assigned(RecordField)
      then RecordField.Free;
  end;

  Items.Clear;
end;

function TMSortingFields.GetCount : integer;
begin
  Result := Items.Count;
end;

procedure TMSortingFields.ResetWithoutElementsFree;
begin
  Items.Clear;
end;


end.
