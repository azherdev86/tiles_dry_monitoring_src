unit FEventlogs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ZDataSet, DBGrids, DB;

type
  TFormEventLogs = class(TForm)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    ButtonRefresh: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
    FQuery : TZQuery;
    procedure RefreshBrowse;
    procedure InitDataBase;
    procedure SetColumnWidth;
  public
    { Public declarations }
  end;

var
  FormEventLogs: TFormEventLogs;

implementation

uses LApplicationGlobals;

{$R *.dfm}

procedure TFormEventLogs.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormEventLogs.ButtonRefreshClick(Sender: TObject);
begin
  RefreshBrowse;
end;

procedure TFormEventLogs.FormCreate(Sender: TObject);
begin
  FQuery := TZQuery.Create(ApplicationDBConnection);
  FQuery.Connection := ApplicationDBConnection;

  InitDataBase;

  RefreshBrowse;
end;

procedure TFormEventLogs.FormDestroy(Sender: TObject);
begin
  FQuery.Active := False;
  FQuery.Free;
end;

procedure TFormEventLogs.RefreshBrowse;
var
  SQLText : string;
begin
  SQLText := 'SELECT EventLogTime as "Time",' + sLineBreak +
                    'EventLogType as "Event type",' + sLineBreak +
                    'EventLogDetails as "Details"' + sLineBreak +
             'FROM EventLogs' + sLineBreak +
             'ORDER BY EventLogId DESC';

  FQuery.Active := False;

  FQuery.SQL.Text := SQLText;

  FQuery.Active := True;

  SetColumnWidth;
end;

procedure TFormEventLogs.InitDataBase;
begin
  DBGrid.DataSource := DataSource;
  DataSource.DataSet := FQuery;
end;

procedure TFormEventLogs.SetColumnWidth;
var
  i, count : integer;
  Column : TColumn;
begin
  count := DBGrid.Columns.Count;

  for i := 0 to count - 1 do
    begin
      Column := DBGrid.Columns[i];

      Column.Title.Alignment := taCenter;

      if not Assigned(Column)
        then Continue;

      if Column.FieldName = 'Event type'
        then Column.Width := 80;

      if Column.FieldName = 'Time'
        then Column.Width := 100;

      if Column.FieldName = 'Details'
        then Column.Width := 1090;

    end;
end;

end.
