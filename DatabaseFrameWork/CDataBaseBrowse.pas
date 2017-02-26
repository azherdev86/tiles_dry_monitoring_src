{*****************************************************************************************
******************************************************************************************
***************В ЭТОМ МОДУЛЕ ОПИСЫВАЮТСЯ ВСЕ ТАБЛИЦЫ, КОТОРЫЕ ОТОБРАЖАЮТСЯ В ПРОЕКТЕ******
******************************************************************************************
******************************************************************************************}
unit CDataBaseBrowse;

interface

uses CTableBrowse;

type
  TMDataBaseBrowse = class
    constructor Create;
    destructor Destroy; override;

  public
    TableBrowses : TMTableBrowses;

    //****************основной метод класса*******
    procedure LoadTableBrowses();
    //********************************************
    function AddTableBrowse(TableName, BrowseName : string) : TMTableBrowse;
    function CopyTableBrowse(SourceBrowseName, NewBrowseName : string) : TMTableBrowse;
    function GetTableBrowse(BrowseName : string) : TMTableBrowse; overload;
    function GetTableBrowse(Index : integer) : TMTableBrowse; overload;
  private
    procedure Reset;
    //задание порядка отображения столбцов в таблицах
    //используется только для тех TableBrowse, где это необходимо
    procedure SetFieldOrders;
    procedure SetFieldWidths; //установка ширины столбцов таблицы
    procedure SetFieldHints;  //установка всплывающих подсказок для столбцов таблицы
    procedure SetVisibleTitles; //установка тех столцбов, заголовки которых не должны быть видны

    //******************************************************************
    //************ МЕТОДЫ, ДЛЯ ЗАДАНИЯ CALCULATED ПОЛЕЙ*****************
    //******************************************************************
    function CONCAT_DB(const StrArray : array of string) : string;

   end;

implementation

uses Dialogs,
     LApplicationGlobals,
     CDataBaseStructure,
     CConditions,
     CQueryConstructor,
     DB,
     CBrowseFields,
     SysUtils,
     CBrowseFieldHints,
     Variants;

constructor TMDataBaseBrowse.Create;
begin
  TableBrowses := TMTableBrowses.Create;

  Reset;
end;

destructor TMDataBaseBrowse.Destroy;
begin
  TableBrowses.Free;
  inherited;
end;

procedure TMDataBaseBrowse.Reset;
begin
  TableBrowses.Reset;
end;


procedure TMDataBaseBrowse.LoadTableBrowses();
begin

  //Установка порядка следования столбцов для тех
  //TableBrowse, для которых это необходимо
  SetFieldOrders;
  //Установка предопределенных условий
  SetFieldWidths;
  //Установка комментариев для столбцов
  SetFieldHints;
  //Установка столбоцов заголовки которых должны быть невидимы
  SetVisibleTitles;
end;

function TMDataBaseBrowse.AddTableBrowse(TableName, BrowseName : string) : TMTableBrowse;
var
  TableBrowse : TMTableBrowse;
begin
  TableBrowse := TMTableBrowse.Create(TableName);
  TableBrowse.BrowseName := BrowseName;

  Result := TableBrowses.AddItem(TableBrowse);

  TableBrowse.AddSortingField(TableBrowse.TableStructure.GetPKField.FieldName, TableName);
end;

function TMDataBaseBrowse.CopyTableBrowse(SourceBrowseName, NewBrowseName : string) : TMTableBrowse;
var
  NewTableBrowse,
  SourceTableBrowse : TMTableBrowse;
begin
  Result := nil;

  SourceTableBrowse := GetTableBrowse(SourceBrowseName);
  if not Assigned(SourceTableBrowse)
    then Exit;

  NewTableBrowse := TMTableBrowse.Create('');
  NewTableBrowse.CopyFromTableBrowse(SourceTableBrowse);
  NewTableBrowse.BrowseName := NewBrowseName;

  Result := TableBrowses.AddItem(NewTableBrowse);
end;


function TMDataBaseBrowse.GetTableBrowse(BrowseName : string) : TMTableBrowse;
begin
  Result := TableBrowses.GetItem(BrowseName);
end;

function TMDataBaseBrowse.GetTableBrowse(Index : integer) : TMTableBrowse;
begin
  Result := TableBrowses.GetItem(Index);
end;

procedure TMDataBaseBrowse.SetFieldOrders;
begin
{Выводятся в запрос Select только те поля, для которых был установлен
FieldOrder > 0.
Важно!!!
- для всех полей TableBrowse должен быть проставлен FieldOrder;
- два поля не могут иметь одинаковый FieldOrder (выведется только одно)  }
  
end;

procedure TMDataBaseBrowse.SetFieldWidths;
begin
  //УКАЗЫВАЮТСЯ ТОЛЬКО ВИДИМЫЕ ПОЛЯ (BrowseField.Visible = TRUE)

end;

procedure TMDataBaseBrowse.SetFieldHints;
begin
  //УКАЗЫВАЮТСЯ ТОЛЬКО ВИДИМЫЕ ПОЛЯ (BrowseField.Visible = TRUE)
  //SetFieldHint - обычная подсказка, независящая от условий
  //AddFieldHint - сложная подсказка, зависящая от условий

end;



procedure TMDataBaseBrowse.SetVisibleTitles;
begin
  //Указываются те поля, заголовки которых должны быть невидимыми

end;


function TMDataBaseBrowse.CONCAT_DB(const StrArray : array of string) : string;
var
  i : integer;
  LowIndex, HighIndex : integer;
  Delimiter : string;
begin
  Result := '';

  LowIndex := Low(StrArray);
  HighIndex := High(StrArray);

  case GLOBAL_DATABASE_TYPE of
    dtMySQL :
      begin
        Result := Result + 'CONCAT(';
        Delimiter := ' , ';
      end;
    dtFireBird :
      begin
        Result := Result + '(';
        Delimiter := ' || ';
      end;
  end;

  for i := LowIndex to HighIndex do
  begin
    Result := Result + StrArray[i];

    if i < HighIndex
      then Result := Result + Delimiter
      else Result := Result + ')';
  end;
end;


end.
