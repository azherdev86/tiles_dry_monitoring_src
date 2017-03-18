unit FDebugPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormDebugPanel = class(TForm)
    ButtonEmulateDataInBase: TButton;
    ButtonResetBase: TButton;
    ButtonStartStopTimers: TButton;
    ButtonSignalState: TButton;
    ButtonEnableSignal: TButton;
    ButtonDisableSignal: TButton;
    MemoInfo: TMemo;
    MemoLogs: TMemo;
    Timer: TTimer;
    ButtonGenerateBoxMessage: TButton;
    Label1: TLabel;
    procedure ButtonStartStopTimersClick(Sender: TObject);
    procedure ButtonEmulateDataInBaseClick(Sender: TObject);
    procedure ButtonResetBaseClick(Sender: TObject);
    procedure ButtonEnableSignalClick(Sender: TObject);
    procedure ButtonDisableSignalClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure SendMessagesClick(Sender: TObject);
    procedure ButtonGenerateBoxMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDebugPanel: TFormDebugPanel;

implementation

uses FMain, CTableRecords, LApplicationGlobals, CController, LUtils,
  FUserDigitalKeyboard;

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

  BoxesNumbers : array [0..19] of Byte;
begin
  //Удаляет все данные и создает датчики

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

  BoxesNumbers[0] := 11;
  BoxesNumbers[1] := 12;
  BoxesNumbers[2] := 21;
  BoxesNumbers[3] := 22;
  BoxesNumbers[4] := 31;
  BoxesNumbers[5] := 32;
  BoxesNumbers[6] := 41;
  BoxesNumbers[7] := 42;
  BoxesNumbers[8] := 51;
  BoxesNumbers[9] := 52;
  BoxesNumbers[10] := 61;
  BoxesNumbers[11] := 62;
  BoxesNumbers[12] := 71;
  BoxesNumbers[13] := 72;
  BoxesNumbers[14] := 81;
  BoxesNumbers[15] := 82;
  BoxesNumbers[16] := 91;
  BoxesNumbers[17] := 92;
  BoxesNumbers[18] := 101;
  BoxesNumbers[19] := 102;

  TableRecord := TMTableRecord.Create('Sensors');
  try
    for i := 0 to 19 do //Коробки
      begin
        BoxNumber := BoxesNumbers[i];
        SectionNumber  := (i div 2) + 1;

        for j := 0 to 4 do //Этажи
          begin
            ConveyorNumber := j + 1;

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

procedure TFormDebugPanel.ButtonEmulateDataInBaseClick(Sender: TObject);
var
  TableRecord : TMTableRecord;
  i, j : integer;
  CurrentDateTime : TDateTime;

  SensorIds : array[1..200] of Integer;
begin
  TableRecord := TMTableRecord.Create('Sensors');
  try
    TableRecord.LoadRecordsAll;

    for i := 0 to TableRecord.RecordsCount - 1 do
      begin
        TableRecord.SetCurrentRecordIndex(i);
        SensorIds[i+1] := TableRecord.PKFieldValue;
      end;
  finally
    TableRecord.Free;
  end;

  TableRecord := TMTableRecord.Create('TempValues');
  try
    CurrentDateTime := Now;

    for i := 1 to 200 do
      begin
        for j := 10*24*60 downto 0 do
        begin
          TableRecord.FieldByName['TempValue'].Value := 140 + Random(25);
          TableRecord.FieldByName['TempTime'].Value  := CurrentDateTime - j*(1/(24*60));
          TableRecord.FieldByName['SensorId'].Value  := SensorIds[i];

          TableRecord.AddRecord;
        end;
        Caption := IntToStr(i);
        Application.ProcessMessages;
      end;
  finally
    TableRecord.Free;
  end;

end;

procedure TFormDebugPanel.ButtonGenerateBoxMessageClick(Sender: TObject);
begin
  FormMain.TimerCreateBoxMessagesTimer(FormMain);
end;

procedure TFormDebugPanel.ButtonStartStopTimersClick(Sender: TObject);
begin
  FormMain.TimerComPortSendMessages.Enabled := not FormMain.TimerComPortSendMessages.Enabled;
//  FormMain.TimerCreateBoxMessages.Enabled := not FormMain.TimerCreateBoxMessages.Enabled;
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

  MemoInfo.Lines.Add('Messages in the queue:' + #9 + IntToStr(QueueCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Messages sent:' + #9 + IntToStr(FormMain.SentMessagesCount));
  MemoInfo.Lines.Add('Messages recieved:' + #9 + IntToStr(FormMain.RecievedMessagesCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Packs recieved:' + #9 + IntToStr(FormMain.RecievedPackCount));
  MemoInfo.Lines.Add('Packs proceed:' + #9 + IntToStr(FormMain.ProceedPackCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Resent count:' + #9 + #9 + #9 + IntToStr(FormMain.ReSentMessagesCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('OutgoingErrorWrongAcknowledge:' + #9  + IntToStr(FormMain.OutgoingErrorWrongAcknowledge));
  MemoInfo.Lines.Add('OutgoingErrorTimeOutCount:' + #9 + #9  + IntToStr(FormMain.OutgoingErrorTimeOutCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('ErrorCRCCount:' + #9 + #9 + #9 + IntToStr(FormMain.IncomingErrorCRCCount));
  MemoInfo.Lines.Add('ErrorNoSendingMessageCount:' + #9 + IntToStr(FormMain.IncomingErrorNoSendingMessageCount));
  MemoInfo.Lines.Add('ErrorSendingMessageCount:' + #9 + #9 + IntToStr(FormMain.IncomingErrorSendingMessageCount));
  MemoInfo.Lines.Add('ErrorBufferOverFlowCout:' + #9 + #9 + IntToStr(FormMain.IncomingErrorBufferOverFlowCout));
  MemoInfo.Lines.Add('ErrorTimeOutEndPacketCount:' + #9 + IntToStr(FormMain.IncomingErrorTimeOutEndPacketCount));
  MemoInfo.Lines.Add('ErrorWrongDeviceId:' + #9 + #9 + IntToStr(FormMain.IncomingErrorWrongDeviceIdCount));
  MemoInfo.Lines.Add('');
end;

end.
