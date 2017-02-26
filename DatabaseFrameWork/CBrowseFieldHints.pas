unit CBrowseFieldHints;

interface

uses Classes, CConditions;

type TMHintType = (htNone,
                   htField,    //комментари€й посто€нен
                   htFieldValue); //комментарий зависит от значени€ пол€


type
  TMHintItem = class
    constructor Create;
    destructor Destroy; override;
  public
    HintText : string;
    FieldValue : string;
    ConditionType : TMConditionType;
  private
    procedure CopyFromHintItem(HintItem : TMHintItem);
    procedure Reset;
    function MatchesCondition(Value : integer) : Boolean;
end;

type
  TMHintItems = class
    constructor Create;
    destructor Destroy; override;

  public
    procedure CopyFromHintItems(HintItems : TMHintItems);
    function GetItem(ItemIndex : integer) : TMHintItem; overload;
    function GetItem(FieldValue : string) : TMHintItem; overload;

    function AddItem(Item : TMHintItem) : TMHintItem;
    function DeleteItem(FieldValue : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;
  end;

type
  TMBrowseFieldHint = class
    constructor Create(hType : TMHintType = htField);
    destructor Destroy; override;
  public
    HintType : TMHintType;
    HintItems : TMHintItems;
    HintText : string;
    procedure CopyFromBrowseFieldHint(BrowseFieldHint : TMBrowseFieldHint);
    procedure AddHint(FieldValue, HintText : string; ConditionType : TMConditionType = ctNone );

    procedure Reset;
  end;


implementation

uses SysUtils;

//*******************TMBrowseFieldHint**************

constructor TMBrowseFieldHint.Create(hType : TMHintType = htField);
begin
  HintItems := TMHintItems.Create;
  Reset;
  HintType := hType;
end;

destructor TMBrowseFieldHint.Destroy;
begin
  Reset;
  HintItems.Free;
  inherited;
end;

procedure TMBrowseFieldHint.Reset;
begin
  HintItems.Reset;
  HintText := '';
end;

procedure TMBrowseFieldHint.AddHint(FieldValue, HintText : string; ConditionType : TMConditionType = ctNone);
var
  HintItem : TMHintItem;
begin
  HintItem := TMHintItem.Create;

  HintItem.FieldValue := FieldValue;
  HintItem.HintText := HintText;
  HintItem.ConditionType := ConditionType;

  HintItems.AddItem(HintItem);
end;

procedure TMBrowseFieldHint.CopyFromBrowseFieldHint(BrowseFieldHint : TMBrowseFieldHint);
begin
  if not Assigned(BrowseFieldHint)
    then Exit;

  HintType := BrowseFieldHint.HintType;
  HintText := BrowseFieldHint.HintText;
  
  HintItems.CopyFromHintItems(BrowseFieldHint.HintItems);
end;


{********************TMHintItem*********************}
constructor TMHintItem.Create;
begin
  Reset;
end;

destructor TMHintItem.Destroy;
begin
  Reset;
  inherited;
end;

procedure TMHintItem.Reset;
begin
  HintText := '';
  FieldValue := '';
  ConditionType := ctNone;
end;

procedure TMHintItem.CopyFromHintItem(HintItem : TMHintItem);
begin
  if not Assigned(HintItem)
    then Exit;

  HintText := HintItem.HintText;
  FieldValue := HintItem.FieldValue;
  ConditionType := HintItem.ConditionType;
end;

function TMHintItem.MatchesCondition(Value : integer) : Boolean;
var
  FieldValueInt : integer;
begin
  Result := False;

  if ConditionType = ctNone
    then Exit;

  FieldValueInt := StrToInt(FieldValue);

  case ConditionType of
    ctEqual     : Result := (Value =  FieldValueInt);
    ctNotEqual  : Result := (Value <> FieldValueInt);
    ctMore      : Result := (Value >  FieldValueInt);
    ctLess      : Result := (Value <  FieldValueInt);
    ctMoreEqual : Result := (Value >= FieldValueInt);
    ctLessEqual : Result := (Value <= FieldValueInt);
  end;                                                  
end;

//****************TMHintItems*****************************
constructor TMHintItems.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMHintItems.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;

function TMHintItems.GetItem(ItemIndex : integer) : TMHintItem;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMHintItem;
end;

procedure TMHintItems.CopyFromHintItems(HintItems : TMHintItems);
var
  i, HintItemsCount : integer;
  NewHintItem,
  OldHintItem : TMHintItem;
begin
  if not Assigned(HintItems)
    then Exit;

  HintItemsCount := HintItems.GetCount;

  for i := 0 to HintItemsCount - 1 do
  begin
    OldHintItem := HintItems.GetItem(i);
    if not Assigned(OldHintItem)
      then Break;

    NewHintItem := TMHintItem.Create;
    NewHintItem.CopyFromHintItem(OldHintItem);
    AddItem(NewHintItem);
  end;
end;


function TMHintItems.GetItem(FieldValue : string) : TMHintItem;
var
  ItemIndex,
  i,
  ItemsCount,
  FieldValueInt : integer;
  Item : TMHintItem;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(FieldValue);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex)
    else
      begin
        ItemsCount := GetCount;

        for i := 0 to ItemsCount - 1 do
        begin
          Item := GetItem(i);

          if not Assigned(Item)
            then Continue;

          if Item.ConditionType = ctNone
            then Continue;

          TryStrToInt(FieldValue,FieldValueInt);
          
          if Item.MatchesCondition(FieldValueInt)
            then
              begin
                Result := Item;
                Break;
              end;
        end;
      end;
end;


function TMHintItems.AddItem(Item : TMHintItem) : TMHintItem;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.FieldValue, Item) >= 0
    then Result := Item;
end;

function TMHintItems.DeleteItem(FieldValue : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(FieldValue);

  Result := DeleteItem(ItemIndex);
end;


function TMHintItems.DeleteItem(ItemIndex : integer) : boolean;
var
  HintItem : TMHintItem;
begin
  Result := FALSE;
  HintItem := GetItem(ItemIndex);
  if Assigned(HintItem) then
    begin
      Items.Delete(ItemIndex);
      HintItem.Free;
      Result := TRUE;
    end;
end;

procedure TMHintItems.Reset;
var
  i : integer;
  Item : TMHintItem;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMHintItem;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;
end;

function TMHintItems.GetCount : integer;
begin
  Result := Items.Count;
end;


end.
