unit CFields;

interface

uses Classes, DB;

type
  TMField = class
    constructor Create;  overload; virtual;
    constructor Create(Field : TMField); overload; virtual;
    destructor Destroy; override;

    public
      FieldName : string;
      FieldType : TFieldType;
      TableName : string; //фактически нужно только для QueryConstructor
      procedure Reset; virtual;
      procedure CopyFromField(Field : TMField);

    private
      function GetFieldString : string;

    public
      property FieldString : string read GetFieldString;
  end;

type
  TMForeignKey = class(TMField)
    public
      ForeignTableName : string;
      procedure Reset; override;
  end;


type
  TMFields = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMField; overload;
    function GetItem(ItemName : string) : TMField; overload;

    function AddItem(Item : TMField) : TMField;
    function DeleteItem(ItemName : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function GetCount : integer;
    procedure Reset;

  private
    Items : TStringList;

  end;



implementation

//////////////TMField//////////////////////////////////

constructor TMField.Create;
begin
  Reset;
end;

constructor TMField.Create(Field : TMField);
begin
  //создан для того чтобы убрался Warning в классе потомке TMRecordFields
end;


destructor TMField.Destroy;
begin
  Reset;
  inherited;
end;

procedure TMField.Reset;
begin
  FieldName := '';
  FieldType := ftUnknown;
  TableName := '';
end;

procedure TMField.CopyFromField(Field : TMField);
begin
  if not Assigned(Field)
    then Exit;

  FieldName := Field.FieldName;
  FieldType := Field.FieldType;
  TableName := Field.TableName;
end;

function TMField.GetFieldString : string;
begin
  Result := TableName + '.' + FieldName;
end;

//////////////TMForeignKey//////////////////////////////////
procedure TMForeignKey.Reset;
begin
  inherited;
  ForeignTableName := '';
end;


//////////////TMFieldSSSSSS//////////////////////////////////
constructor TMFields.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMFields.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;


function TMFields.GetItem(ItemIndex : integer) : TMField;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMField;
end;


function TMFields.GetItem(ItemName : string) : TMField;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemName);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMFields.AddItem(Item : TMField) : TMField;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.FieldName, Item) >= 0
    then Result := Item;
end;

function TMFields.DeleteItem(ItemName : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemName);

  Result := DeleteItem(ItemIndex);
end;


function TMFields.DeleteItem(ItemIndex : integer) : boolean;
var
  Field : TMField;
begin
  Result := FALSE;
  Field := GetItem(ItemIndex);
  if Assigned(Field) then
    begin
      Items.Delete(ItemIndex);
      Field.Free;
      Result := TRUE;
    end;
end;

procedure TMFields.Reset;
var
  i, count : integer;
  Field : TMField;
begin
  count := GetCount;

  for i := 0 to count - 1 do
  begin
    Field := Items.Objects[i] as TMField;
    if Assigned(Field)
      then Field.Free;
  end;

  Items.Clear;
end;

function TMFields.GetCount : integer;
begin
  Result := Items.Count;
end;


end.
