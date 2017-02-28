unit FInputPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormInputPassword = class(TForm)
    LabeledEditPassword: TLabeledEdit;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    PasswordIsCorrect : boolean;
  end;

function InputPassword() : boolean;

var
  FormInputPassword: TFormInputPassword;

implementation

uses LApplicationGlobals, LHash;


{$R *.dfm}

function InputPassword() : boolean;
begin
  Application.CreateForm(TFormInputPassword, FormInputPassword);

  FormInputPassword.ShowModal;

  Result := FormInputPassword.PasswordIsCorrect;

  FormInputPassword.Free;
end;

procedure TFormInputPassword.ButtonCancelClick(Sender: TObject);
begin
  PasswordIsCorrect := False;

  Close;
end;

procedure TFormInputPassword.ButtonOkClick(Sender: TObject);
var
  Pass : string;
  Hash : string;
begin
//  Pass := LabeledEditPassword.Text;

//  Hash := ApplicationProgramSettings.

//if MD5DigestToStr(MD5String(Pass)) = ''
//    then PasswordIsCorrect := True;

  PasswordIsCorrect := True;

  Close;
end;

procedure TFormInputPassword.FormCreate(Sender: TObject);
begin
  PasswordIsCorrect := False;
end;

end.
