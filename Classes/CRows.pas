unit CRows;

interface

uses Classes;

type
  TMColumn = class
    private
      FColumnIndex : integer;

    public
      property ColumnIndex : integer read FColumnIndex write FColumnIndex;
  end;

type
  TMRow = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMColumn;  overload;
    function GetItem(ItemTitle : string) : TMColumn; overload;

    function AddItem(Item : TMColumn) : TMColumn;

    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

    procedure Clear;

  private
    Items : TStringList;
    FRowIndex : integer;

    procedure Reset;

  public
    property RowIndex : Integer read FRowIndex write FRowIndex;
  end;


type
  TMRows = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMRow;  overload;
    function GetItem(ItemTitle : string) : TMRow; overload;

    function AddItem(Item : TMRow) : TMRow;

    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

  private
    Items : TStringList;

    procedure Reset;

  end;


implementation

uses SysUtils;

//////////////TMRow//////////////////////////////////
constructor TMRow.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMRow.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMRow.GetItem(ItemIndex : integer) : TMColumn;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMColumn;
end;

function TMRow.GetItem(ItemTitle : string) : TMColumn;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMRow.AddItem(Item : TMColumn) : TMColumn;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

   if Items.AddObject(IntToStr(Item.FColumnIndex), Item) >= 0
    then Result := Item;
end;


function TMRow.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMColumn;
begin
  Result := FALSE;
  Item := GetItem(ItemIndex);
  if Assigned(Item) then
    begin
      Items.Delete(ItemIndex);
      Item.Free;
      Result := TRUE;
    end;
end;

function TMRow.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


procedure TMRow.Reset;
var
  i, count : integer;
  Item : TMColumn;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    Item := Items.Objects[i] as TMColumn;
    if Assigned(Item)
      then Item.Free;
  end;

  FRowIndex := 0;

  Items.Clear;
end;

function TMRow.GetCount : integer;
begin
  Result := Items.Count;
end;

procedure TMRow.Clear;
begin
  Reset;
end;

//////////////TMRows//////////////////////////////////
constructor TMRows.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMRows.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMRows.GetItem(ItemIndex : integer) : TMRow;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMRow;
end;

function TMRows.GetItem(ItemTitle : string) : TMRow;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMRows.AddItem(Item : TMRow) : TMRow;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

   if Items.AddObject(IntToStr(Item.FRowIndex), Item) >= 0
    then Result := Item;
end;


function TMRows.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMRow;
begin
  Result := FALSE;
  Item := GetItem(ItemIndex);
  if Assigned(Item) then
    begin
      Items.Delete(ItemIndex);
      Item.Free;
      Result := TRUE;
    end;
end;

function TMRows.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


procedure TMRows.Reset;
var
  i, count : integer;
  Item : TMRow;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    Item := Items.Objects[i] as TMRow;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;
end;

function TMRows.GetCount : integer;
begin
  Result := Items.Count;
end;

end.
