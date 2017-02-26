unit CBrowseFieldImages;

interface

uses Classes, CConditions;

type TMImageType = (itNone,
                    itOnSelect,           //картинки сменяются при наведении
                    itFieldValue,         //картинка зависит от значения поля
                    itReferense,          //ссылка
                    itProgressBar,        //прогресс-бар
                    itExtendedProgressBar //расширенный прогресс бар (используется в проектах)
                    );


type
  TMImageItem = class
    constructor Create;
    destructor Destroy; override;
  public
    SelectedImageIndex : integer;
    DefaultImageIndex : integer;
    FieldValue : string;
    ConditionType : TMConditionType;
  private
    procedure CopyFromImageItem(ImageItem : TMImageItem);
    procedure Reset;
    function MatchesCondition(Value : integer) : Boolean;
end;

type
  TMImageItems = class
    constructor Create;
    destructor Destroy; override;

  public
    procedure CopyFromImageItems(ImageItems : TMImageItems);
    function GetItem(ItemIndex : integer) : TMImageItem; overload;
    function GetItem(FieldValue : string) : TMImageItem; overload;

    function AddItem(Item : TMImageItem) : TMImageItem;
    function DeleteItem(FieldValue : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;
  end;

type
  TMBrowseFieldImage = class
    constructor Create(iType : TMImageType = itOnSelect);
    destructor Destroy; override;
  public
    ImageType : TMImageType;
    ImageItems : TMImageItems;
    DefaultImageIndex : integer;
    SelectedImageIndex : integer;

    CurrentProgressFieldTitle : string;
    ExtendedProgressFieldTitle : string;
    MaxValueFieldTitle : string;
    procedure CopyFromBrowseFieldImage(BrowseFieldImage : TMBrowseFieldImage);
    procedure AddImage(FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone );
    procedure Reset;
  end;


implementation

uses SysUtils;

//*******************TMBrowseFieldImage**************

constructor TMBrowseFieldImage.Create(iType : TMImageType = itOnSelect);
begin
  ImageItems := TMImageItems.Create;
  Reset;
  ImageType := iType;
end;

destructor TMBrowseFieldImage.Destroy;
begin
  Reset;
  ImageItems.Free;
  inherited;
end;

procedure TMBrowseFieldImage.Reset;
begin
  ImageItems.Reset;
  ImageType := itNone;
  DefaultImageIndex := -1;
  SelectedImageIndex := -1;

  CurrentProgressFieldTitle := '';
  MaxValueFieldTitle := '';
  ExtendedProgressFieldTitle := '';
end;

procedure TMBrowseFieldImage.AddImage(FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone );
var
  ImageItem : TMImageItem;
begin
  ImageItem := TMImageItem.Create;

  ImageItem.FieldValue := FieldValue;
  ImageItem.SelectedImageIndex := SelectedImageIndex;
  ImageItem.DefaultImageIndex := DefaultImageIndex;
  ImageItem.ConditionType := ConditionType;

  ImageItems.AddItem(ImageItem);
end;

procedure TMBrowseFieldImage.CopyFromBrowseFieldImage(BrowseFieldImage : TMBrowseFieldImage);
begin
  if not Assigned(BrowseFieldImage)
    then Exit;

  ImageType                  := BrowseFieldImage.ImageType;
  DefaultImageIndex          := BrowseFieldImage.DefaultImageIndex;
  SelectedImageIndex         := BrowseFieldImage.SelectedImageIndex;
  CurrentProgressFieldTitle  := BrowseFieldImage.CurrentProgressFieldTitle;
  MaxValueFieldTitle         := BrowseFieldImage.MaxValueFieldTitle;
  ExtendedProgressFieldTitle := BrowseFieldImage.ExtendedProgressFieldTitle;

  ImageItems.CopyFromImageItems(BrowseFieldImage.ImageItems);
end;


{********************TMImageItem*********************}
constructor TMImageItem.Create;
begin
  Reset;
end;

destructor TMImageItem.Destroy;
begin
  Reset;
  inherited;
end;

procedure TMImageItem.Reset;
begin
  SelectedImageIndex := -1;
  DefaultImageIndex := -1;
  FieldValue := '';
  ConditionType := ctNone;
end;

procedure TMImageItem.CopyFromImageItem(ImageItem : TMImageItem);
begin
  if not Assigned(ImageItem)
    then Exit;

  SelectedImageIndex := ImageItem.SelectedImageIndex;
  DefaultImageIndex := ImageItem.DefaultImageIndex;
  FieldValue := ImageItem.FieldValue;
  ConditionType := ImageItem.ConditionType;
end;


function TMImageItem.MatchesCondition(Value : integer) : Boolean;
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

//****************TMImageItems*****************************
constructor TMImageItems.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMImageItems.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;


function TMImageItems.GetItem(ItemIndex : integer) : TMImageItem;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMImageItem;
end;

procedure TMImageItems.CopyFromImageItems(ImageItems : TMImageItems);
var
  i, ImageItemsCount : integer;
  NewImageItem,
  OldImageItem : TMImageItem;
begin
  if not Assigned(ImageItems)
    then Exit;

  ImageItemsCount := ImageItems.GetCount;

  for i := 0 to ImageItemsCount - 1 do
  begin
    OldImageItem := ImageItems.GetItem(i);

    if not Assigned(OldImageItem)
      then break;

    NewImageItem := TMImageItem.Create;
    NewImageItem.CopyFromImageItem(OldImageItem);
    AddItem(NewImageItem);    
  end;
    

end;


function TMImageItems.GetItem(FieldValue : string) : TMImageItem;
var
  ItemIndex,
  i,
  ItemsCount,
  FieldValueInt : integer;
  Item : TMImageItem;
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

          FieldValueInt := StrToInt(FieldValue);

          if Item.MatchesCondition(FieldValueInt)
            then
              begin
                Result := Item;
                Break;
              end;
        end;                
      end;
end;


function TMImageItems.AddItem(Item : TMImageItem) : TMImageItem;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.FieldValue, Item) >= 0
    then Result := Item;
end;

function TMImageItems.DeleteItem(FieldValue : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(FieldValue);

  Result := DeleteItem(ItemIndex);
end;


function TMImageItems.DeleteItem(ItemIndex : integer) : boolean;
var
  Table : TMImageItem;
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

procedure TMImageItems.Reset;
var
  i : integer;
  Item : TMImageItem;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMImageItem;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;
end;

function TMImageItems.GetCount : integer;
begin
  Result := Items.Count;
end;


end.
