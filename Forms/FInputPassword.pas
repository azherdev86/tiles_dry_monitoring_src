unit FInputPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FTerminalForm, Buttons;

type
  TypePasswordMode = (pmNone,
                      pmCorrect,
                      pmWrong);

type
  TFormInputPassword = class(TFormTerminal)
    LabeledEditPassword: TLabeledEdit;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure LabeledEditPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtnOkClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    PasswordMode : TypePasswordMode;
  end;

function InputPassword() : TypePasswordMode;

var
  FormInputPassword: TFormInputPassword;

implementation

uses LApplicationGlobals, LHash;


{$R *.dfm}

function InputPassword() : TypePasswordMode;
begin
  Application.CreateForm(TFormInputPassword, FormInputPassword);

  FormInputPassword.ShowModal;

  Result := FormInputPassword.PasswordMode;

  FormInputPassword.Free;
end;

procedure TFormInputPassword.BitBtnCancelClick(Sender: TObject);
begin
  PasswordMode := pmNone;

  Close;
end;

procedure TFormInputPassword.BitBtnOkClick(Sender: TObject);
var
  Pass : string;
  Hash : string;
begin
  PasswordMode := pmWrong;

  Pass := LabeledEditPassword.Text;

  if Pass = ''
    then
      begin
        ShowMessage('Password required');
        LabeledEditPassword.SetFocus;
        Exit;
      end;

  Hash := ApplicationProgramSettings.UserSettings.PasswordHash;

  if MD5DigestToStr(MD5String(Pass)) <> Hash
    then
      begin
        ShowMessage('Wrong password');
        LabeledEditPassword.SetFocus;
        Exit;
      end;

  PasswordMode := pmCorrect;

  Close;
end;

procedure TFormInputPassword.FormCreate(Sender: TObject);
begin
  inherited;

  PasswordMode := pmNone;
end;

procedure TFormInputPassword.LabeledEditPasswordKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: BitBtnOk.Click;
    VK_ESCAPE:
      begin
        PasswordMode := pmNone;
        Close;
      end;
  end;
end;

end.
