unit CBrowseFields;

interface

uses Classes,
     CFields,
     CBrowseFieldHints,
     CBrowseFieldImages,
     CConditions;

//Функция для объекта TMBrowseField - позволяет задать для различных BrowseField свой метод
//для определения строки, которая выведется в табличке RTableBrowse
type TMGetFieldTextFunction = function (InParam : variant) : string of object;

type TMBrowseFieldTypes = (bftData,  //обычные FieldName as "FieldTitle"
                           bftDataFunction, //текст, который рассчитывается в специальных
                                            //функциях (поле на основе fbtData, т.к. исходное
                                            //значение берется из данных (не рассчитывается)
                           bftGraphic, //графические 1 as "FieldTitle"
                           bftDataGraphic, //графичекие, в которых картинка зависит от значеия в ячейке
                           bftCalculated, //CONCAT, CASE
                           bftCalculatedFunction, //аналог bftDataFunction но на основе
                                                  //bftCalculated
                           bftAgregate,  //Агрегатные функции
                           bftColorText); //цветной текст 

type
  TMBrowseFields = class; //forward declaration
  TMBrowseField = class (TMField)
    constructor Create(); override;
    destructor Destroy; override;
    public
      BrowseFieldType : TMBrowseFieldTypes;

      Visible       : boolean;   //по умолчанию TRUE (поле видимо)
      FieldTitle    : string;
      TitleVisible  : boolean; //по умолчанию TRUE (заголов поля видим)

      FieldOrder    : integer; //порядковый номер поля (по умолачинию - 0),
                            //если задан порядок, то от 1 до N
      FieldWidth    : integer; // 0 - автоширина, остальное в пикселах

      Hint          : TMBrowseFieldHint; //всплывающая подсказка
      Image         : TMBrowseFieldImage; //картинка

      //если поле нестандартное, а содержит в себе  какую нибудь функцию
      //COUNT(*), MIN(), MAX() и т.п. нужно записывать SQL код этого запроса
      QueryText : string;

      GetFieldText: TMGetFieldTextFunction;

      procedure Reset; override;
      procedure CopyFromBrowseField(BrowseField : TMBrowseField);

      procedure AddHint(FieldValue, HintText : string; ConditionType : TMConditionType = ctNone);
      procedure AddReferense();
      procedure AddProgressBar(CurrentProgressFieldTitle, MaxValueFieldTitle : string);
      procedure AddExtendedProgressBar(CurrentProgressFieldTitle, ExtendedProgressFieldTitle, MaxValueFieldTitle : string);
      procedure AddImage(SelectedImageIndex : integer; DefaultImageIndex : integer = -1; iType : TMImageType = itOnSelect); overload;
      procedure AddImage(FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone); overload;
    private
      function GetTitleString() : string;


    public
      property TitleString : string read GetTitleString;
  end;

  TMBrowseFields = class
    constructor Create;
    destructor Destroy; override;
    destructor DestroyWithoutElementsFree;  //деструктор без удаления Items
  public

    function GetItem(ItemIndex : integer) : TMBrowseField; overload;
    function GetItem(ItemTitle : string) : TMBrowseField; overload;

    function AddItem(Item : TMBrowseField) : TMBrowseField;
    function AddItems(BrowseFields : TMBrowseFields) : integer;

    function DeleteItem(ItemTitle : string) : boolean; overload;
    function DeleteItem(ItemIndex : integer) : boolean; overload;

    function DropItem(ItemIndex : integer) : boolean; overload;
    function DropItem(ItemTitle : string) : boolean; overload;
    function DropItem(Item : TMBrowseField) : boolean; overload;
    procedure CopyFromBrowseFields(BrowseFields : TMBrowseFields);



    function GetCount : integer;
    procedure Reset;

  private
    procedure ResetWithoutElementsFree;

  private
    Items : TStringList;
    FMaxFieldOrder : integer;

  public
    property MaxFieldOrder : Integer read FMaxFieldOrder;
  end;

implementation

uses LApplicationGlobals,
     SysUtils,
     CDataBaseStructure,
     variants;

constructor TMBrowseField.Create();
begin
  Hint := TMBrowseFieldHint.Create;
  Image := TMBrowseFieldImage.Create;
  inherited;
end;


destructor TMBrowseField.Destroy;
begin
  inherited;
  Hint.Free;
  Image.Free;
end;

procedure TMBrowseField.Reset;
begin
  inherited;
  GetFieldText:= nil;
  Visible := TRUE;
  FieldTitle := '';
  FieldOrder := 0;
  QueryText := '';
  BrowseFieldType := bftData;
  TitleVisible := TRUE;
  FieldWidth := 0;
  Hint.Reset;
  Image.Reset;
end;


procedure TMBrowseField.AddHint(FieldValue, HintText : string; ConditionType : TMConditionType = ctNone);
begin
  Hint.AddHint(FieldValue, HintText, ConditionType);
  Hint.HintType := htFieldValue;
end;


procedure TMBrowseField.AddImage(FieldValue : string; SelectedImageIndex : integer; DefaultImageIndex : integer = -1; ConditionType : TMConditionType = ctNone);
begin
  Image.AddImage(FieldValue, SelectedImageIndex, DefaultImageIndex, ConditionType);
  Image.ImageType := itFieldValue;
end;

procedure TMBrowseField.CopyFromBrowseField(BrowseField : TMBrowseField);
begin
  if not Assigned(BrowseField)
    then Exit;


  GetFieldText := BrowseField.GetFieldText;
  BrowseFieldType := BrowseField.BrowseFieldType;
  Visible := BrowseField.Visible;
  FieldTitle := BrowseField.FieldTitle;
  TitleVisible := BrowseField.TitleVisible;
  FieldOrder  := BrowseField.FieldOrder;

  FieldWidth := BrowseField.FieldWidth;
  QueryText := BrowseField.QueryText;

  Hint.CopyFromBrowseFieldHint(BrowseField.Hint);
  Image.CopyFromBrowseFieldImage(BrowseField.Image);


  CopyFromField(BrowseField as TMField);
end;

procedure TMBrowseField.AddReferense();
begin
  Image.ImageType := itReferense;
end;

procedure TMBrowseField.AddProgressBar(CurrentProgressFieldTitle, MaxValueFieldTitle : string);
begin
  Image.CurrentProgressFieldTitle := CurrentProgressFieldTitle;
  Image.MaxValueFieldTitle        := MaxValueFieldTitle;

  Image.ImageType                 := itProgressBar;
end;

procedure TMBrowseField.AddExtendedProgressBar(CurrentProgressFieldTitle, ExtendedProgressFieldTitle, MaxValueFieldTitle : string);
begin
  Image.CurrentProgressFieldTitle  := CurrentProgressFieldTitle;
  Image.MaxValueFieldTitle         := MaxValueFieldTitle;
  Image.ExtendedProgressFieldTitle := ExtendedProgressFieldTitle;

  Image.ImageType                  := itExtendedProgressBar;
end;

procedure TMBrowseField.AddImage(SelectedImageIndex : integer; DefaultImageIndex : integer = -1; iType : TMImageType = itOnSelect);
begin
  Image.SelectedImageIndex := SelectedImageIndex;
  Image.DefaultImageIndex  := DefaultImageIndex;
  Image.ImageType          := iType;
end;

function TMBrowseField.GetTitleString() : string;
begin
  //Выводим BrowseField в зависимости от типа
  case BrowseFieldType of
    bftData, bftDataGraphic, bftDataFunction, bftColorText: Result := TableName + '.' + FieldName;
    bftGraphic: Result := '1111';
    bftCalculated, bftCalculatedFunction, bftAgregate:
      begin
        Result := QueryText;
        case GLOBAL_DATABASE_TYPE of
          dtFireBird :
            begin
              QueryText := StringReplace(QueryText, '"', '''', [rfReplaceAll]);
              Result := QueryText;
            end;
          dtMySQL : Result := QueryText;
        end;
      end;
  end;


  //Если заголовок поля задан, то
  if FieldTitle <> ''
    then Result := Result + ' as ' + '"' + FieldTitle + '"';
end;

//////////////TMBrowseFieldsSSSSSSS//////////////////////////////////
constructor TMBrowseFields.Create;
begin
  Items := TStringList.Create;
  Reset;
end;

destructor TMBrowseFields.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;

end;


function TMBrowseFields.GetItem(ItemIndex : integer) : TMBrowseField;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMBrowseField;
end;

function TMBrowseFields.AddItems(BrowseFields : TMBrowseFields) : integer;
var
  i,
  BrowseFieldsCount : integer;
  BrowseField : TMBrowseField;
begin
  BrowseFieldsCount := BrowseFields.GetCount;
  Result := 0;

  for i := 0 to BrowseFieldsCount - 1 do
  begin
    BrowseField := BrowseFields.GetItem(i);

    if not Assigned(BrowseField)
      then Continue;

    if Assigned(AddItem(BrowseField))
      then INC(Result);
  end;
end;


function TMBrowseFields.GetItem(ItemTitle : string) : TMBrowseField;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMBrowseFields.AddItem(Item : TMBrowseField) : TMBrowseField;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.FieldTitle, Item) >= 0
    then Result := Item;

  if Item.FieldOrder > FMaxFieldOrder
    then FMaxFieldOrder := Item.FieldOrder;  
end;

function TMBrowseFields.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


function TMBrowseFields.DeleteItem(ItemIndex : integer) : boolean;
var
  RecordField : TMBrowseField;
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

function TMBrowseFields.DropItem(ItemIndex : integer) : boolean;
begin
  result := true;
  Items.Delete(ItemIndex);
end;

function TMBrowseFields.DropItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DropItem(ItemIndex);
end;


function TMBrowseFields.DropItem(Item : TMBrowseField) : boolean;
begin
  Result := DropItem(Item.FieldTitle);
end;

procedure TMBrowseFields.CopyFromBrowseFields(BrowseFields : TMBrowseFields);
var
  i, FieldsCount : integer;
  NewBrowseField,
  OldBrowseField : TMBrowseField;
begin
  if not Assigned(BrowseFields)
    then Exit;

  FieldsCount := BrowseFields.GetCount;

  for i := 0 to FieldsCount - 1 do
  begin
    OldBrowseField := BrowseFields.GetItem(i);
    if not Assigned(OldBrowseField)
      then Break;

    NewBrowseField := TMBrowseField.Create;
    NewBrowseField.CopyFromBrowseField(OldBrowseField);
    AddItem(NewBrowseField);
  end;
end;


procedure TMBrowseFields.Reset;
var
  i : integer;
  RecordField : TMBrowseField;
begin
  for i := 0 to GetCount - 1 do
  begin
    RecordField := Items.Objects[i] as TMBrowseField;
    if Assigned(RecordField)
      then RecordField.Free;
  end;

  Items.Clear;

  FMaxFieldOrder := 0;
end;

function TMBrowseFields.GetCount : integer;
begin
  Result := Items.Count;
end;

destructor TMBrowseFields.DestroyWithoutElementsFree;
begin
  ResetWithoutElementsFree;
  Items.Free;
  inherited Destroy;
end;

procedure TMBrowseFields.ResetWithoutElementsFree;
begin
  Items.Clear;
  FMaxFieldOrder := 0;
end;

end.
