unit FInputPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FTerminalForm;

type
  TypePasswordMode = (pmNone,
                      pmCorrect,
                      pmWrong);

type
  TFormInputPassword = class(TFormTerminal)
    LabeledEditPassword: TLabeledEdit;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure LabeledEditPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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

procedure TFormInputPassword.ButtonCancelClick(Sender: TObject);
begin
  PasswordMode := pmNone;

  Close;
end;

procedure TFormInputPassword.ButtonOkClick(Sender: TObject);
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
    VK_RETURN: ButtonOk.Click;
    VK_ESCAPE:
      begin
        PasswordMode := pmNone;
        Close;
      end;
  end;
end;

end.
