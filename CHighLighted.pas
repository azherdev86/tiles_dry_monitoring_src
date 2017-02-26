unit CHighLighted;

interface

uses Classes;

type
  TMHighLightedColumn = class
    private
      FColumnIndex : integer;

    public
      property ColumnIndex : integer read FColumnIndex write FColumnIndex;
  end;

type
  TMHighLightedRow = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMHighLightedColumn;  overload;
    function GetItem(ItemTitle : string) : TMHighLightedColumn; overload;

    function AddItem(Item : TMHighLightedColumn) : TMHighLightedColumn;

    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

    procedure Clear;

  private
    Items : TStringList;
    FRowIndex : integer;

    procedure Reset;
    function IsHighlighted : boolean;

  public
    property RowIndex : Integer read FRowIndex write FRowIndex;
    property HighLighted : boolean read IsHighLighted;
  end;


type
  TMHighLightedRows = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMHighLightedRow;  overload;
    function GetItem(ItemTitle : string) : TMHighLightedRow; overload;

    function AddItem(Item : TMHighLightedRow) : TMHighLightedRow;

    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

    procedure Clear;

  private
    Items : TStringList;

    procedure Reset;
    function IsHighLighted : boolean;

  public
    property HighLighted : boolean read IsHighlighted;
  end;




implementation

uses SysUtils;

//////////////TMHighLightedRow//////////////////////////////////
constructor TMHighLightedRow.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMHighLightedRow.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMHighLightedRow.GetItem(ItemIndex : integer) : TMHighLightedColumn;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMHighLightedColumn;
end;

function TMHighLightedRow.GetItem(ItemTitle : string) : TMHighLightedColumn;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMHighLightedRow.AddItem(Item : TMHighLightedColumn) : TMHighLightedColumn;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

   if Items.AddObject(IntToStr(Item.FColumnIndex), Item) >= 0
    then Result := Item;
end;


function TMHighLightedRow.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMHighLightedColumn;
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

function TMHighLightedRow.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


procedure TMHighLightedRow.Reset;
var
  i : integer;
  Item : TMHighLightedColumn;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMHighLightedColumn;
    if Assigned(Item)
      then Item.Free;
  end;

  FRowIndex := -1;

  Items.Clear;
end;

function TMHighLightedRow.IsHighlighted : boolean;
begin
  Result := GetCount > 0;
end;

function TMHighLightedRow.GetCount : integer;
begin
  Result := Items.Count;
end;


procedure TMHighLightedRow.Clear;
begin
  Reset;
end;


//////////////TMHighLightedRows//////////////////////////////////
constructor TMHighLightedRows.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMHighLightedRows.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMHighLightedRows.GetItem(ItemIndex : integer) : TMHighLightedRow;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMHighLightedRow;
end;

function TMHighLightedRows.GetItem(ItemTitle : string) : TMHighLightedRow;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMHighLightedRows.AddItem(Item : TMHighLightedRow) : TMHighLightedRow;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

   if Items.AddObject(IntToStr(Item.FRowIndex), Item) >= 0
    then Result := Item;
end;


function TMHighLightedRows.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMHighLightedRow;
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

function TMHighLightedRows.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


procedure TMHighLightedRows.Reset;
var
  i : integer;
  Item : TMHighLightedRow;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMHighLightedRow;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;
end;

function TMHighLightedRows.GetCount : integer;
begin
  Result := Items.Count;
end;


procedure TMHighLightedRows.Clear;
begin
  Reset;
end;

function TMHighLightedRows.IsHighlighted : boolean;
begin
  Result := GetCount > 0;
end;


end.
