unit FEventHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls;

type
  TFormEventHistory = class(TForm)
    StringGrid1: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEventHistory: TFormEventHistory;

implementation

{$R *.dfm}

procedure TFormEventHistory.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TFormEventHistory.FormCreate(Sender: TObject);
begin
  Position := poDesktopCenter;

  StringGrid1.Cells[0, 0] := 'Time';
  StringGrid1.Cells[1, 0] := 'Event type';
  StringGrid1.Cells[2, 0] := 'Details';

  StringGrid1.Cells[0, 1] := '18.02.2017 09:00:13';
  StringGrid1.Cells[1, 1] := 'Operation mode ON';
  StringGrid1.Cells[2, 1] := 'Floor 3';

  StringGrid1.Cells[0, 2] := '18.02.2017 08:04:11';
  StringGrid1.Cells[1, 2] := 'Operation mode ON';
  StringGrid1.Cells[2, 2] := 'Floor 4';

  StringGrid1.Cells[0, 3] := '18.02.2017 07:30:25';
  StringGrid1.Cells[1, 3] := 'Operation mode ON';
  StringGrid1.Cells[2, 3] := 'Floor 5';

  StringGrid1.Cells[0, 4] := '18.02.2017 07:15:53';
  StringGrid1.Cells[1, 4] := 'Program started';
  StringGrid1.Cells[2, 4] := '';
end;

end.
