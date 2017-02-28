unit FTemperatureRanges;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFormTemperatureRanges = class(TForm)
    ButtonApply: TButton;
    ButtonCancel: TButton;
    LabeledEditSection1MinYValue: TLabeledEdit;
    LabeledEditSection1MaxYValue: TLabeledEdit;
    LabeledEditSection2MinYValue: TLabeledEdit;
    LabeledEditSection2MaxYValue: TLabeledEdit;
    LabeledEditSection3MinYValue: TLabeledEdit;
    LabeledEditSection3MaxYValue: TLabeledEdit;
    LabeledEditSection4MinYValue: TLabeledEdit;
    LabeledEditSection4MaxYValue: TLabeledEdit;
    LabeledEditSection5MinYValue: TLabeledEdit;
    LabeledEditSection5MaxYValue: TLabeledEdit;
    LabelSection1: TLabel;
    LabelSection2: TLabel;
    LabelSection4: TLabel;
    LabelSection3: TLabel;
    LabelSection5: TLabel;
    LabeledEditSection6MinYValue: TLabeledEdit;
    LabeledEditSection6MaxYValue: TLabeledEdit;
    LabeledEditSection7MinYValue: TLabeledEdit;
    LabeledEditSection7MaxYValue: TLabeledEdit;
    LabeledEditSection8MinYValue: TLabeledEdit;
    LabeledEditSection8MaxYValue: TLabeledEdit;
    LabeledEditSection9MinYValue: TLabeledEdit;
    LabeledEditSection9MaxYValue: TLabeledEdit;
    LabeledEditSection10MinYValue: TLabeledEdit;
    LabeledEditSection10MaxYValue: TLabeledEdit;
    LabelSection6: TLabel;
    LabelSection7: TLabel;
    LabelSection9: TLabel;
    LabelSection8: TLabel;
    LabelSection10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    function LoadSettings() : boolean;
    function SaveSettings() : boolean;
    function CheckValues()  : boolean;
    function GetLabelEdit(ALabelEditName : string) : TLabeledEdit;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTemperatureRanges: TFormTemperatureRanges;

implementation

{$R *.dfm}

uses LApplicationGlobals;

procedure TFormTemperatureRanges.ButtonApplyClick(Sender: TObject);
begin
  if not CheckValues
    then Exit;

  SaveSettings;
  Close;
end;

procedure TFormTemperatureRanges.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormTemperatureRanges.FormCreate(Sender: TObject);
begin
  LoadSettings;

  Position := poDesktopCenter;
end;

function TFormTemperatureRanges.LoadSettings() : boolean;
begin
  LabeledEditSection1MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section1RangeMinYValue);
  LabeledEditSection1MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section1RangeMaxYValue);

  LabeledEditSection2MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section2RangeMinYValue);
  LabeledEditSection2MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section2RangeMaxYValue);

  LabeledEditSection3MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section3RangeMinYValue);
  LabeledEditSection3MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section3RangeMaxYValue);

  LabeledEditSection4MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section4RangeMinYValue);
  LabeledEditSection4MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section4RangeMaxYValue);

  LabeledEditSection5MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section5RangeMinYValue);
  LabeledEditSection5MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section5RangeMaxYValue);

  LabeledEditSection6MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section6RangeMinYValue);
  LabeledEditSection6MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section6RangeMaxYValue);

  LabeledEditSection7MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section7RangeMinYValue);
  LabeledEditSection7MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section7RangeMaxYValue);

  LabeledEditSection8MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section8RangeMinYValue);
  LabeledEditSection8MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section8RangeMaxYValue);

  LabeledEditSection9MinYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section9RangeMinYValue);
  LabeledEditSection9MaxYValue.Text  := IntToStr(ApplicationProgramSettings.GraphSettings.Section9RangeMaxYValue);

  LabeledEditSection10MinYValue.Text := IntToStr(ApplicationProgramSettings.GraphSettings.Section10RangeMinYValue);
  LabeledEditSection10MaxYValue.Text := IntToStr(ApplicationProgramSettings.GraphSettings.Section10RangeMaxYValue);

  Result := TRUE;
end;


function TFormTemperatureRanges.SaveSettings() : boolean;
begin
  ApplicationProgramSettings.GraphSettings.Section1RangeMinYValue := StrToIntDef(LabeledEditSection1MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section1RangeMaxYValue := StrToIntDef(LabeledEditSection1MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section2RangeMinYValue := StrToIntDef(LabeledEditSection2MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section2RangeMaxYValue := StrToIntDef(LabeledEditSection2MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section3RangeMinYValue := StrToIntDef(LabeledEditSection3MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section3RangeMaxYValue := StrToIntDef(LabeledEditSection3MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section4RangeMinYValue := StrToIntDef(LabeledEditSection4MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section4RangeMaxYValue := StrToIntDef(LabeledEditSection4MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section5RangeMinYValue := StrToIntDef(LabeledEditSection5MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section5RangeMaxYValue := StrToIntDef(LabeledEditSection5MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section6RangeMinYValue := StrToIntDef(LabeledEditSection6MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section6RangeMaxYValue := StrToIntDef(LabeledEditSection6MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section7RangeMinYValue := StrToIntDef(LabeledEditSection7MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section7RangeMaxYValue := StrToIntDef(LabeledEditSection7MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section8RangeMinYValue := StrToIntDef(LabeledEditSection8MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section8RangeMaxYValue := StrToIntDef(LabeledEditSection8MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section9RangeMinYValue := StrToIntDef(LabeledEditSection9MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section9RangeMaxYValue := StrToIntDef(LabeledEditSection9MaxYValue.Text, 0);

  ApplicationProgramSettings.GraphSettings.Section10RangeMinYValue := StrToIntDef(LabeledEditSection10MinYValue.Text, 0);
  ApplicationProgramSettings.GraphSettings.Section10RangeMaxYValue := StrToIntDef(LabeledEditSection10MaxYValue.Text, 0);

  ApplicationGraph.LoadSettings;

  Result := ApplicationProgramSettings.SaveToInifile;
end;

function TFormTemperatureRanges.CheckValues() : boolean;
var
  i : integer;

  LabelEditMin,
  LabelEditMax : TLabeledEdit;

  component_name : string;

  min_value,
  max_value : single;
begin
  Result := True;

  for i := 1 to 10 do
    begin
      component_name := 'LabeledEditSection' + IntToStr(i) + 'MinYValue';
      LabelEditMin := GetLabelEdit(component_name);

      component_name := 'LabeledEditSection' + IntToStr(i) + 'MaxYValue';
      LabelEditMax := GetLabelEdit(component_name);

      if (not Assigned(LabelEditMin)) or (not Assigned(LabelEditMax))
        then
          begin
            ShowMessage('Unknown error');

            Result := False;
            break;
          end;

      if not TryStrToFloat(LabelEditMin.Text, min_value, ApplicationFormatSettings)
        then
          begin
            ShowMessage('Incorrect input data');
            LabelEditMin.SetFocus;
            LabelEditMin.SelectAll;

            Result := False;
            break;
          end;

      if not TryStrToFloat(LabelEditMax.Text, max_value, ApplicationFormatSettings)
        then
          begin
            ShowMessage('Incorrect input data');
            LabelEditMax.SetFocus;
            LabelEditMax.SelectAll;

            Result := False;
            break;
          end;

      if min_value >= max_value
        then
          begin
            ShowMessage('Min value should be less than Max');

            LabelEditMin.SetFocus;
            LabelEditMin.SelectAll;

            Result := False;
            break;
          end;
    end;

end;

function TFormTemperatureRanges.GetLabelEdit(ALabelEditName : string) : TLabeledEdit;
var
  count, i : integer;

  Component : TComponent;
begin
  Result := nil;

  count := ComponentCount;

  for i := 0 to count - 1 do
    begin
      Component := Components[i];

      if not Assigned(Component)
        then Continue;

      if not (Component is TLabeledEdit)
        then Continue;

      if Component.Name = ALabelEditName
        then
          begin
            Result := Component as TLabeledEdit;

            break;
          end;
    end;
end;

end.
