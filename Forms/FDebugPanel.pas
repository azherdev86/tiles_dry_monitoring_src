unit FDebugPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormDebugPanel = class(TForm)
    ButtonGenerateBase: TButton;
    ButtonResetBase: TButton;
    ButtonStartStopTimers: TButton;
    ButtonSignalState: TButton;
    ButtonEnableSignal: TButton;
    ButtonDisableSignal: TButton;
    MemoInfo: TMemo;
    MemoLogs: TMemo;
    Timer: TTimer;
    SendMessages: TButton;
    procedure ButtonStartStopTimersClick(Sender: TObject);
    procedure ButtonGenerateBaseClick(Sender: TObject);
    procedure ButtonResetBaseClick(Sender: TObject);
    procedure ButtonEnableSignalClick(Sender: TObject);
    procedure ButtonDisableSignalClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure SendMessagesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDebugPanel: TFormDebugPanel;

implementation

uses FMain, CTableRecords, LApplicationGlobals, CController;

{$R *.dfm}

procedure TFormDebugPanel.ButtonResetBaseClick(Sender: TObject);
var
  TableRecord : TMTableRecord;
  i, j, k,
  BoxNumber,
  ConveyorNumber,
  SectionNumber : integer;
  SensorPosition,
  SensorSide : string;
begin
  TableRecord := TMTableRecord.Create('TempValues');
  try
    TableRecord.DeleteRecordsAll;
  finally
    TableRecord.Free;
  end;


  TableRecord := TMTableRecord.Create('Sensors');
  try
    TableRecord.DeleteRecordsAll;
  finally
    TableRecord.Free;
  end;


  TableRecord := TMTableRecord.Create('EventLogs');
  try
    TableRecord.DeleteRecordsAll;
  finally
    TableRecord.Free;
  end;

  TableRecord := TMTableRecord.Create('Sensors');
  try
    for i := 0 to 19 do //Коробки
      begin
        for j := 0 to 4 do //Этажи
          begin
            BoxNumber      := i + 1;  //НЕПРАВИЛЬНО! Сейчас такой порядок 11 12 21 22 31 32 и т.п.
            ConveyorNumber := j + 1;
            SectionNumber  := ((BoxNumber - 1) div 2) + 1;

            if (BoxNumber mod 2) = 1
              then SensorSide := 'Left'
              else SensorSide := 'Right';

            for k := 0 to 1 do
              begin
                if k = 0
                  then SensorPosition := SensorSide + 'Bottom'
                  else SensorPosition := SensorSide + 'Top';

                TableRecord.FieldByName['BoxNumber'].Value      := BoxNumber;
                TableRecord.FieldByName['ConveyorNumber'].Value := ConveyorNumber;
                TableRecord.FieldByName['SectionNumber'].Value  := SectionNumber;
                TableRecord.FieldByName['SensorPosition'].Value := SensorPosition;

                TableRecord.AddRecord;
              end;
          end;
      end;
  finally
    TableRecord.Free;
  end;

end;

procedure TFormDebugPanel.ButtonDisableSignalClick(Sender: TObject);
begin
  ApplicationController.GenerateSetSignalModeMessage(smDisabled);
end;

procedure TFormDebugPanel.ButtonEnableSignalClick(Sender: TObject);
begin
  ApplicationController.GenerateSetSignalModeMessage(smEnabled);
end;

procedure TFormDebugPanel.ButtonGenerateBaseClick(Sender: TObject);
var
  TableRecord : TMTableRecord;
  i, j : integer;
  CurrentDateTime : TDateTime;
begin
  TableRecord := TMTableRecord.Create('TempValues');
  try
    CurrentDateTime := Now;

    for i := 1 to 200 do
      begin
        for j := 10*24*60 downto 0 do
        begin
          TableRecord.FieldByName['TempValue'].Value := 140 + Random(25);
          TableRecord.FieldByName['TempTime'].Value  := CurrentDateTime - j*(1/(24*60));
          TableRecord.FieldByName['SensorId'].Value  := i;

          TableRecord.AddRecord;
        end;
        Caption := IntToStr(i);
        Application.ProcessMessages;
      end;
  finally
    TableRecord.Free;
  end;

end;

procedure TFormDebugPanel.ButtonStartStopTimersClick(Sender: TObject);
begin
  FormMain.TimerComPortSendMessages.Enabled := not FormMain.TimerComPortSendMessages.Enabled;
  FormMain.TimerCreateBoxMessages.Enabled := not FormMain.TimerCreateBoxMessages.Enabled;
end;

procedure TFormDebugPanel.SendMessagesClick(Sender: TObject);
begin
  FormMain.TimerComPortSendMessagesTimer(FormMain);
end;

procedure TFormDebugPanel.TimerTimer(Sender: TObject);
var
  QueueCount : integer;
begin
  QueueCount := ApplicationComPortOutgoingMessages.GetCount;

  MemoInfo.Clear;

  MemoInfo.Lines.Add('Сообщений в очереди:' + #9 + IntToStr(QueueCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Отправлено сообщений:' + #9 + IntToStr(FormMain.SentMessagesCount));
  MemoInfo.Lines.Add('Получено сообщений:' + #9 + IntToStr(FormMain.RecievedMessagesCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Получено пачек:' + #9 + #9 + IntToStr(FormMain.RecievedPackCount));
  MemoInfo.Lines.Add('Обработано пачек:' + #9 + IntToStr(FormMain.ProceedPackCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Повторных отправок:' + #9 + #9 + IntToStr(FormMain.ReSentMessagesCount));
  MemoInfo.Lines.Add('ErrorCRCCount:' + #9 + #9 + #9 + IntToStr(FormMain.ErrorCRCCount));
  MemoInfo.Lines.Add('ErrorNoSendingMessageCount:' + #9 + IntToStr(FormMain.ErrorNoSendingMessageCount));
  MemoInfo.Lines.Add('ErrorBufferOverFlowCout:' + #9 + #9 + IntToStr(FormMain.ErrorBufferOverFlowCout));
  MemoInfo.Lines.Add('ErrorTimeOutEndPacketCount:' + #9 + IntToStr(FormMain.ErrorTimeOutEndPacketCount));
  MemoInfo.Lines.Add('ErrorWrongCmdOrDeviceId:' + #9 + #9 + IntToStr(FormMain.ErrorWrongCmdOrDeviceId));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('ErrorTimeOutSendPacketCount:' + #9 + IntToStr(FormMain.ErrorTimeOutSendPacketCount));

end;

end.
