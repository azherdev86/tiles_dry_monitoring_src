unit FControlPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormControlPanel = class(TForm)
    BitBtnTurnOffComputer: TBitBtn;
    BitBtnChangePassword: TBitBtn;
    BitBtnCloseProgram: TBitBtn;
    BitBtnCancel: TBitBtn;
    Label1: TLabel;
    procedure BitBtnChangePasswordClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnCloseProgramClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnTurnOffComputerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NeedClose : boolean;
  end;

var
  FormControlPanel: TFormControlPanel;

implementation

uses FChangePassword, ShellAPI;

{$R *.dfm}

procedure TFormControlPanel.BitBtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormControlPanel.BitBtnChangePasswordClick(Sender: TObject);
begin
  Application.CreateForm(TFormChangePassword, FormChangePassword);
  FormChangePassword.ShowModal;
  FormChangePassword.Free;
end;

procedure TFormControlPanel.BitBtnCloseProgramClick(Sender: TObject);
begin
  NeedClose := True;
  Close;
end;

procedure TFormControlPanel.BitBtnTurnOffComputerClick(Sender: TObject);
begin
  if MessageDlg('Continue and turn off the computer?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
    then ShellExecute(handle, nil,'shutdown',' -s -t 00','', SW_SHOWNORMAL);
end;

procedure TFormControlPanel.FormCreate(Sender: TObject);
begin
  NeedClose := False;
end;

end.
