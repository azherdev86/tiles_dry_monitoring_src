{*****************************************************************************************
******************************************************************************************
***************� ���� ������ ����������� ��� �������, ������� ������������ � �������******
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

    //****************�������� ����� ������*******
    procedure LoadTableBrowses();
    //********************************************
    function AddTableBrowse(TableName, BrowseName : string) : TMTableBrowse;
    function CopyTableBrowse(SourceBrowseName, NewBrowseName : string) : TMTableBrowse;
    function GetTableBrowse(BrowseName : string) : TMTableBrowse; overload;
    function GetTableBrowse(Index : integer) : TMTableBrowse; overload;
  private
    procedure Reset;
    //������� ������� ����������� �������� � ��������
    //������������ ������ ��� ��� TableBrowse, ��� ��� ����������
    procedure SetFieldOrders;
    procedure SetFieldWidths; //��������� ������ �������� �������
    procedure SetFieldHints;  //��������� ����������� ��������� ��� �������� �������
    procedure SetVisibleTitles; //��������� ��� ��������, ��������� ������� �� ������ ���� �����

    //******************************************************************
    //************ ������, ��� ������� CALCULATED �����*****************
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

  //��������� ������� ���������� �������� ��� ���
  //TableBrowse, ��� ������� ��� ����������
  SetFieldOrders;
  //��������� ���������������� �������
  SetFieldWidths;
  //��������� ������������ ��� ��������
  SetFieldHints;
  //��������� ��������� ��������� ������� ������ ���� ��������
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
{��������� � ������ Select ������ �� ����, ��� ������� ��� ����������
FieldOrder > 0.
�����!!!
- ��� ���� ����� TableBrowse ������ ���� ���������� FieldOrder;
- ��� ���� �� ����� ����� ���������� FieldOrder (��������� ������ ����)  }
  
end;

procedure TMDataBaseBrowse.SetFieldWidths;
begin
  //����������� ������ ������� ���� (BrowseField.Visible = TRUE)

end;

procedure TMDataBaseBrowse.SetFieldHints;
begin
  //����������� ������ ������� ���� (BrowseField.Visible = TRUE)
  //SetFieldHint - ������� ���������, ����������� �� �������
  //AddFieldHint - ������� ���������, ��������� �� �������

end;



procedure TMDataBaseBrowse.SetVisibleTitles;
begin
  //����������� �� ����, ��������� ������� ������ ���� ����������

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
