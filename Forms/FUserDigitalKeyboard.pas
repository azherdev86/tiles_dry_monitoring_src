unit FUserDigitalKeyboard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TypeResultOperation = (roNone,
                         roEnter,
                         roEsc);

type
  TFormUserDigitalKeyboard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ButtonCLR: TButton;
    ButtonDel: TButton;
    ButtonEnter: TButton;
    ButtonEsc: TButton;
    ButtonDecimalSeparator: TButton;
    Button0: TButton;
    EditResult: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button0Click(Sender: TObject);
    procedure ButtonDecimalSeparatorClick(Sender: TObject);
    procedure ButtonEscClick(Sender: TObject);
    procedure ButtonCLRClick(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure ButtonEnterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditResultClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ResultValue     : single;
    ResultOperation : TypeResultOperation;
  end;

var
  FormUserDigitalKeyboard: TFormUserDigitalKeyboard;

implementation

uses LApplicationGlobals;

{$R *.dfm}

procedure TFormUserDigitalKeyboard.Button0Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '0';
end;

procedure TFormUserDigitalKeyboard.Button1Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '1';
end;

procedure TFormUserDigitalKeyboard.Button2Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '2';
end;

procedure TFormUserDigitalKeyboard.Button3Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '3';
end;

procedure TFormUserDigitalKeyboard.Button4Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

    EditResult.Text := EditResult.Text + '4';
end;

procedure TFormUserDigitalKeyboard.Button5Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '5';
end;

procedure TFormUserDigitalKeyboard.Button6Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '6';
end;

procedure TFormUserDigitalKeyboard.Button7Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '7';
end;

procedure TFormUserDigitalKeyboard.Button8Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '8';
end;

procedure TFormUserDigitalKeyboard.Button9Click(Sender: TObject);
begin
  if EditResult.Text = '0'
    then EditResult.Text := '';

  EditResult.Text := EditResult.Text + '9';
end;

procedure TFormUserDigitalKeyboard.ButtonCLRClick(Sender: TObject);
begin
  EditResult.Text := '0';
end;

procedure TFormUserDigitalKeyboard.ButtonDecimalSeparatorClick(Sender: TObject);
var
  DecimalSeparator : string;
begin
  DecimalSeparator := ApplicationFormatSettings.DecimalSeparator;

  if Pos(DecimalSeparator, EditResult.Text) > 0
    then Exit;

  EditResult.Text := EditResult.Text + DecimalSeparator;
end;

procedure TFormUserDigitalKeyboard.ButtonDelClick(Sender: TObject);
var
  str : string;
begin
  str := EditResult.Text;

  SetLength(str, Length(str) - 1);

  if Length(str) = 0 
    then EditResult.Text := '0'
    else EditResult.Text := str;  
end;

procedure TFormUserDigitalKeyboard.ButtonEnterClick(Sender: TObject);
begin
  ResultValue := StrToFloatDef(EditResult.Text, 0, ApplicationFormatSettings);
  ResultOperation := roEnter;
  Close;
end;

procedure TFormUserDigitalKeyboard.ButtonEscClick(Sender: TObject);
begin
  ResultOperation := roEsc;
  Close;
end;

procedure TFormUserDigitalKeyboard.EditResultClick(Sender: TObject);
begin
  HideCaret(EditResult.Handle);
end;

procedure TFormUserDigitalKeyboard.FormCreate(Sender: TObject);
begin
  ResultOperation := roNone;
  HideCaret(EditResult.Handle);
end;

end.
