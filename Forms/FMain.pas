unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CPort, StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs, Chart, Buttons,
  ComCtrls, CIncomingComPortMessage, COutgoingComPortMessage, FTerminalForm;

type
  TFormMain = class(TFormTerminal)
    PaintBox: TPaintBox;
    TimerCreateBoxMessages: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    LabelAxisMinYValue: TLabel;
    LabelAxisMaxYValue: TLabel;
    Label25: TLabel;
    GroupBoxFloorAxisSettings: TGroupBox;
    LabeledEditAxisMinYValue: TLabeledEdit;
    LabeledEditAxisMaxYValue: TLabeledEdit;
    BitBtnEventHistory: TBitBtn;
    BitBtnTemperatureRanges: TBitBtn;
    ButtonApplyFloorAxisSettings: TButton;
    Label19: TLabel;
    StatusBar: TStatusBar;
    ComPort: TComPort;
    TimerComPortSendMessages: TTimer;
    TimerRefreshView: TTimer;
    ImageGraphLegend: TImage;
    TrackBarConveyor1: TTrackBar;
    LabelConveyor1Test: TLabel;
    LabelConveyor1Work: TLabel;
    TrackBarConveyor2: TTrackBar;
    LabelConveyor2Test: TLabel;
    LabelConveyor2Work: TLabel;
    TrackBarConveyor3: TTrackBar;
    LabelConveyor3Test: TLabel;
    LabelConveyor3Work: TLabel;
    TrackBarConveyor4: TTrackBar;
    LabelConveyor4Test: TLabel;
    LabelConveyor4Work: TLabel;
    TrackBarConveyor5: TTrackBar;
    LabelConveyor5Test: TLabel;
    LabelConveyor5Work: TLabel;
    TrackBarAllConveyors: TTrackBar;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    BitBtbExportToCSV: TBitBtn;
    ButtonDebug: TButton;
    TimerCreateCheckSignaModelMessages: TTimer;
    LabelConveyorAllWork: TLabel;
    LabelConveyorAllTest: TLabel;
    BitBtnSirenDisable: TBitBtn;
    BitBtnControlPanel: TBitBtn;
    TimerScheduler: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BitBtnTemperatureRangesClick(Sender: TObject);
    procedure BitBtnEventHistoryClick(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure TimerCreateBoxMessagesTimer(Sender: TObject);
    procedure TimerComPortSendMessagesTimer(Sender: TObject);
    procedure TimerRefreshViewTimer(Sender: TObject);
    procedure TimerSchedulerTimer(Sender: TObject);
    procedure ButtonApplyFloorAxisSettingsClick(Sender: TObject);
    procedure LabeledEditAxisMinYValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEditAxisMaxYValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrackBarConveyor5Change(Sender: TObject);
    procedure TrackBarConveyor4Change(Sender: TObject);
    procedure TrackBarConveyor3Change(Sender: TObject);
    procedure TrackBarConveyor2Change(Sender: TObject);
    procedure TrackBarConveyor1Change(Sender: TObject);
    procedure TrackBarAllConveyorsChange(Sender: TObject);
    procedure BitBtbExportToCSVClick(Sender: TObject);
    procedure ButtonDebugClick(Sender: TObject);
    procedure TimerCreateCheckSignaModelMessagesTimer(Sender: TObject);
    procedure BitBtnControlPanelClick(Sender: TObject);
    procedure BitBtnSirenDisableClick(Sender: TObject);
  private
    { Private declarations }
    FStartTime : double;
    FGraphMouseCoord : TPoint;

    //флаги
    FNeedDeleteOutdatedTempValues : boolean; //Нужно удалять устаревшие значения (да/нет)
    FOutdatedTempValuesDeleted : boolean;     //Устаревшие значения удалены (да/нет)

    FNeedBackupOutdatedTempValues : boolean; //Нужно сохранять устаревшие значения (да/нет)
    FOutdatedTempValuesBackuped : boolean;     //Устаревшие значения сохранены (да/нет)

    FNeedDeleteComPortMessages : boolean; //Нужно удалять устаревшие сообщения из базы (да/нет)
    FComPortMessagesDeleted : boolean;     //Устаревшие сообщения удалены (да/нет)

    procedure UpdateGraph();
    procedure UpdateStatusBar;
    procedure UpdateSignalMode;

    function LoadSettings : boolean;
    function SaveSettings : boolean;

    procedure ClearMessage(var SendingMessage : TMOutgoingComportMessage);

    function GraphMouseToGridCoord(AMouseCoord : TPoint; out AConveyorNumber, ASectionNumber : integer) : boolean;
    function ProcessIncomingMessage(IncomingMessage : TMIncomingComportMessage) : boolean;
    function ProcessIncomingMessageErrors(IncomingMessage : TMIncomingComportMessage) : boolean;

    function DeleteOutdatedTempValues() : boolean;
    function BackupOutdatedTempValues() : boolean;
    function DeleteOutdatedComPortMessages() : boolean;

  public
    { Public declarations }
    SentMessagesCount           : integer;
    RecievedMessagesCount       : integer;

    RecievedPackCount           : integer;
    ProceedPackCount            : integer;

    ReSentMessagesCount         : integer;

    IncomingErrorCRCCount                : integer;
    IncomingErrorNoSendingMessageCount   : integer;
    IncomingErrorSendingMessageCount     : integer;
    IncomingErrorWrongDeviceIdCount      : integer;
    IncomingErrorBufferOverFlowCout      : integer;
    IncomingErrorTimeOutEndPacketCount   : integer;

    OutgoingErrorWrongAcknowledge : integer;
    OutgoingErrorTimeOutCount     : integer;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses LApplicationGlobals, CGraph, ShellAPI, FTemperatureRanges, FEventLogs,
     FGraphHistory, CBoxes, CBasicComPortMessage, DateUtils, CTableRecords, ZDataset,
     CTempValuesBuffer, CController, FInputPassword, FChangePassword, FExportToCSV,
     CEventLog, LUtils, FDebugPanel, CQueryConstructor, CConditions, CExportToCSV,
  FUserDigitalKeyboard, FControlPanel;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;

  FGraphMouseCoord := Point(0, 0);

  FormMain.WindowState := wsMaximized;
  FormMain.DoubleBuffered := TRUE;
  UpdateGraph;

  FStartTime := Now;

  SentMessagesCount           := 0;
  ReSentMessagesCount         := 0;
  RecievedMessagesCount       := 0;
  RecievedPackCount           := 0;
  ProceedPackCount            := 0;
  IncomingErrorCRCCount                := 0;
  IncomingErrorNoSendingMessageCount   := 0;
  IncomingErrorSendingMessageCount     := 0;
  IncomingErrorBufferOverFlowCout      := 0;
  IncomingErrorTimeOutEndPacketCount   := 0;
  IncomingErrorWrongDeviceIdCount      := 0;

  OutgoingErrorWrongAcknowledge := 0;
  OutgoingErrorTimeOutCount     := 0;

  FNeedDeleteOutdatedTempValues := False;
  FOutdatedTempValuesDeleted    := False;

  FNeedBackupOutdatedTempValues := False;
  FOutdatedTempValuesBackuped   := False;

  FNeedDeleteComPortMessages    := False;
  FComPortMessagesDeleted       := False;

  LoadSettings;

  UpdateStatusBar;

  try
    ComPort.Connected := TRUE;
  except
    ShowMessage('Couldn''t connect to COM port' + sLineBreak +
                'Check device or program settings.');
    ApplicationEventLog.WriteLog(elComPortError, 'Couldn''t connect to COM port during start application');
    Application.Terminate;
  end;

  TimerCreateBoxMessagesTimer(Self);
  TimerRefreshViewTimer(Self);

  ButtonDebug.Visible := ApplicationProgramSettings.UserSettings.DebugMode;
end;

procedure TFormMain.LabeledEditAxisMaxYValueKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN : ButtonApplyFloorAxisSettings.Click;
  end;
end;

procedure TFormMain.LabeledEditAxisMinYValueKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN : ButtonApplyFloorAxisSettings.Click;
  end;
end;

procedure TFormMain.PaintBoxClick(Sender: TObject);
var
  ConveyorNumber,
  SectionNumber : integer;
begin
  if not GraphMouseToGridCoord(FGraphMouseCoord, ConveyorNumber, SectionNumber)
    then Exit;

  Application.CreateForm(TFormGraphHistory, FormGraphHistory);

  FormGraphHistory.ConveyorNumber := ConveyorNumber;
  FormGraphHistory.SectionNumber  := SectionNumber;

  FormGraphHistory.ShowModal;
  FormGraphHistory.Free;
end;

procedure TFormMain.PaintBoxMouseEnter(Sender: TObject);
begin
  ApplicationGraph.NeedDrawSelectedSection := TRUE;

  PaintBox.Repaint;
end;

procedure TFormMain.PaintBoxMouseLeave(Sender: TObject);
begin
  ApplicationGraph.NeedDrawSelectedSection := FALSE;
  PaintBox.Repaint;
end;

procedure TFormMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  CurrentSection : TPoint;
begin
  FGraphMouseCoord := Point(X, Y);

  CurrentSection := Point(X div GRID_COL_WIDTH, Y div GRID_ROW_HEIGHT);

  ApplicationGraph.SelectedSection := CurrentSection;

  PaintBox.Repaint;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
begin
  ApplicationGraph.DrawGraph(PaintBox);
end;

procedure TFormMain.TimerCreateBoxMessagesTimer(Sender: TObject);
var
  ComPortMessage : TMOutgoingComportMessage;
  Box : TMBox;
  i, count : integer;
begin
  //1 раз в минуту (настройка таймера), формируется пачка сообщений для отправки
  //сообщения формируются мгновенно, но будут отправлены из ComPortSendMessageTimer
  //здесь сообщения помещаются в очередь для отправки
  count := ApplicationBoxes.GetCount;

  if ApplicationComPortOutgoingMessages.GetCount >= ApplicationComPortOutgoingMessages.MaxMessagesCount
    then Exit; //Если в очереди скопилось много неотправленных сообщений, то мы не создаем новые


  for i := 0 to {0} count - 1 do
  begin
    ComPortMessage := TMOutgoingComportMessage.Create;

    Box := ApplicationBoxes.GetItem(i);

    if not Assigned(Box)
      then Continue;

    if not Box.SaveToComPortMessage(ComPortMessage)
      then ComPortMessage.Free;

    if Assigned(ComPortMessage)
      then ApplicationComPortOutgoingMessages.AddItem(ComPortMessage);
  end;

//  TimerCreateComPortMessages.Enabled := False;
end;

procedure TFormMain.TimerCreateCheckSignaModelMessagesTimer(Sender: TObject);
begin
  if ApplicationComPortOutgoingMessages.GetCount > MAX_MESSAGES_COUNT
    then Exit;

  ApplicationController.GenerateCheckSignalModeMessage;
end;

procedure TFormMain.TimerRefreshViewTimer(Sender: TObject);
begin
  ApplicationController.CheckTemperatureRanges;

  UpdateGraph;

  UpdateStatusBar;

  BitBtnSirenDisable.Enabled := (ApplicationController.SignalMode = smEnabled);
end;

procedure TFormMain.TimerSchedulerTimer(Sender: TObject);
const
  CDeleteValuesHour   = 0;
  CBackupValuesHour   = 1;
  CDeleteMessagesHour = 2;
begin
  ///////////////// Удаление старых даных ////////////////////////
  ////////////////// Каждый день в 00:00 /////////////////////////
  if (DecodeHour(Now) = CDeleteValuesHour) and (not FOutdatedTempValuesDeleted) //если настало нужное время и операция еще не выполнена
    then FNeedDeleteOutdatedTempValues := True; //то выставляем флаг

  if FNeedDeleteOutdatedTempValues and (not FOutdatedTempValuesDeleted)
    then FOutdatedTempValuesDeleted := DeleteOutdatedTempValues();

  if FOutdatedTempValuesDeleted
    then FNeedDeleteOutdatedTempValues := False;

  if (DecodeHour(Now) > CDeleteValuesHour)
    then FOutdatedTempValuesDeleted := False;

  ///////////////// Сохранение старых данных ////////////////////////
  ////////////////// Каждый день в 01:00 /////////////////////////
  if (DecodeHour(Now) = CBackupValuesHour) and (not FOutdatedTempValuesBackuped) //если настало нужное время и операция еще не выполнена
    then FNeedBackupOutdatedTempValues := True; //то выставляем флаг

  if FNeedBackupOutdatedTempValues and (not FOutdatedTempValuesBackuped)
    then FOutdatedTempValuesBackuped := BackupOutdatedTempValues();

  if FOutdatedTempValuesBackuped
    then FNeedBackupOutdatedTempValues := False;

  if (DecodeHour(Now) > CBackupValuesHour)
    then FOutdatedTempValuesBackuped := False;

  ///////////////// Сохранение старых данных ////////////////////////
  ////////////////// Каждый день в 02:00 /////////////////////////
  if (DecodeHour(Now) = CDeleteMessagesHour) and (not FComPortMessagesDeleted) //если настало нужное время и операция еще не выполнена
    then FNeedDeleteComPortMessages := True; //то выставляем флаг

  if FNeedDeleteComPortMessages and (not FComPortMessagesDeleted)
    then FComPortMessagesDeleted := DeleteOutdatedComPortMessages();

  if FComPortMessagesDeleted
    then FNeedDeleteComPortMessages := False;

  if (DecodeHour(Now) > CDeleteMessagesHour)
    then FComPortMessagesDeleted := False;
end;

procedure TFormMain.TrackBarAllConveyorsChange(Sender: TObject);
var
  position : integer;
begin
  position := 0;

  case TrackBarAllConveyors.Position of
    0 : position := 0;
    1 : position := 1;
  end;

  TrackBarConveyor1.Position := position;
  TrackBarConveyor2.Position := position;
  TrackBarConveyor3.Position := position;
  TrackBarConveyor4.Position := position;
  TrackBarConveyor5.Position := position;
end;

procedure TFormMain.TrackBarConveyor1Change(Sender: TObject);
var
  Conveyor : TMConveyor;
begin
  Conveyor := ApplicationController.GetItem('1');

  case TrackBarConveyor1.Position of
    0 :
      begin
        if Conveyor.WorkMode = cwmOverlocking
          then Exit;

        Conveyor.WorkMode := cwmOverlocking;
      end;
    1 :
      begin
        if Conveyor.WorkMode = cwmWork
          then Exit;

        Conveyor.WorkMode := cwmWork;
      end;
  end;

  case Conveyor.WorkMode of
    cwmOverlocking : ApplicationEventLog.WriteLog(elWorkModeOff, 'floor 1');
    cwmWork        : ApplicationEventLog.WriteLog(elWorkModeOn , 'floor 1');
  end;
end;

procedure TFormMain.TrackBarConveyor2Change(Sender: TObject);
var
  Conveyor : TMConveyor;
begin
  Conveyor := ApplicationController.GetItem('2');

  case TrackBarConveyor2.Position of
    0 :
      begin
        if Conveyor.WorkMode = cwmOverlocking
          then Exit;

        Conveyor.WorkMode := cwmOverlocking;
      end;
    1 :
      begin
        if Conveyor.WorkMode = cwmWork
          then Exit;

        Conveyor.WorkMode := cwmWork;
      end;
  end;

  case Conveyor.WorkMode of
    cwmOverlocking : ApplicationEventLog.WriteLog(elWorkModeOff, 'floor 2');
    cwmWork        : ApplicationEventLog.WriteLog(elWorkModeOn , 'floor 2');
  end;
end;

procedure TFormMain.TrackBarConveyor3Change(Sender: TObject);
var
  Conveyor : TMConveyor;
begin
  Conveyor := ApplicationController.GetItem('3');

  case TrackBarConveyor3.Position of
    0 :
      begin
        if Conveyor.WorkMode = cwmOverlocking
          then Exit;

        Conveyor.WorkMode := cwmOverlocking;
      end;
    1 :
      begin
        if Conveyor.WorkMode = cwmWork
          then Exit;

        Conveyor.WorkMode := cwmWork;
      end;
  end;

  case Conveyor.WorkMode of
    cwmOverlocking : ApplicationEventLog.WriteLog(elWorkModeOff, 'floor 3');
    cwmWork        : ApplicationEventLog.WriteLog(elWorkModeOn , 'floor 3');
  end;
end;

procedure TFormMain.TrackBarConveyor4Change(Sender: TObject);
var
  Conveyor : TMConveyor;
begin
  Conveyor := ApplicationController.GetItem('4');

  case TrackBarConveyor4.Position of
    0 :
      begin
        if Conveyor.WorkMode = cwmOverlocking
          then Exit;

        Conveyor.WorkMode := cwmOverlocking;
      end;
    1 :
      begin
        if Conveyor.WorkMode = cwmWork
          then Exit;

        Conveyor.WorkMode := cwmWork;
      end;
  end;

  case Conveyor.WorkMode of
    cwmOverlocking : ApplicationEventLog.WriteLog(elWorkModeOff, 'floor 4');
    cwmWork        : ApplicationEventLog.WriteLog(elWorkModeOn , 'floor 4');
  end;
end;

procedure TFormMain.TrackBarConveyor5Change(Sender: TObject);
var
  Conveyor : TMConveyor;
begin
  Conveyor := ApplicationController.GetItem('5');

  case TrackBarConveyor5.Position of
    0 :
      begin
        if Conveyor.WorkMode = cwmOverlocking
          then Exit;

        Conveyor.WorkMode := cwmOverlocking;
      end;
    1 :
      begin
        if Conveyor.WorkMode = cwmWork
          then Exit;

        Conveyor.WorkMode := cwmWork;
      end;
  end;

  case Conveyor.WorkMode of
    cwmOverlocking : ApplicationEventLog.WriteLog(elWorkModeOff, 'floor 5');
    cwmWork        : ApplicationEventLog.WriteLog(elWorkModeOn , 'floor 5');
  end;
end;

procedure TFormMain.TimerComPortSendMessagesTimer(Sender: TObject);
var
  SendingMessage : TMOutgoingComportMessage;
  MessageBytes : TDynamicByteArray;
begin
  SendingMessage := ApplicationComPortOutgoingMessages.SendingComPortMessage;

  if (ApplicationComPortOutgoingMessages.GetCount = 0) and
     (not Assigned(SendingMessage))
    then Exit; //Если нечего отсылать - выходим

  if Assigned(SendingMessage) //Если есть отправленное сообщение
    then
      begin
        if SendingMessage.State = omsDelievered
          then
            if not SendingMessage.IsError
              then ClearMessage(SendingMessage); //SendingMessage := nil

        if Assigned(SendingMessage)
          then
            begin
              if not SendingMessage.IsError //Если нет ошибок, проверяем
                then
                  if not SendingMessage.IsTimeOutError //Проверка таймаута
                    then Exit;

              if SendingMessage.IsError
                then
                  begin
                    case SendingMessage.Error of
                      omeWrongAcknowledge : INC(OutgoingErrorWrongAcknowledge);
                      omeTimeout          : INC(OutgoingErrorTimeOutCount);
                    end;
                  end;

              if SendingMessage.SentTimeCounter < MESSAGE_RESEND_COUNT
                then //Если есть зафиксированные ошибки, сообщение отправляется повторно
                  begin
                    SendingMessage.ResentPrepare;
                    Inc(ReSentMessagesCount);
                  end
                else
                  begin
                    ClearMessage(SendingMessage);
                    Exit;
                  end;
            end;
      end;

  if not Assigned(SendingMessage) //Если сообщение не создано ранее, т.е. не идет повторная отправка
  //то мы берем сообщение из очереди в соответствии со временем создания и приоритетом
    then SendingMessage := ApplicationComPortOutgoingMessages.GetMessageToSend;

  if not Assigned(SendingMessage)
    then Exit;

  SendingMessage.GenerateMessage(MessageBytes);

  try
    if ComPort.Write(MessageBytes[0], Length(MessageBytes)) > 0
      then
        begin
          SendingMessage.State           := omsWaitResponse;
          SendingMessage.SentTime        := Now;

          ApplicationComPortOutgoingMessages.SendingComPortMessage := SendingMessage;
          SendingMessage.SaveToDataBase;

          Inc(SentMessagesCount);
        end;
  except
    ApplicationEventLog.WriteLog(elComPortError, 'Can''t send message to the device');
  end;
end;


procedure TFormMain.BitBtnSirenDisableClick(Sender: TObject);
begin
  ApplicationController.GenerateSetSignalModeMessage(smDisabled);
  ApplicationEventLog.WriteLog(elSignalOff, 'by user');
end;

procedure TFormMain.BitBtbExportToCSVClick(Sender: TObject);
begin
  Application.CreateForm(TFormExportToCSV, FormExportToCSV);
  FormExportToCSV.ShowModal;
  FormExportToCSV.Free;
end;

procedure TFormMain.BitBtnControlPanelClick(Sender: TObject);
var
  ExitProgram : boolean;
begin
  Application.CreateForm(TFormControlPanel, FormControlPanel);
  try
    FormControlPanel.ShowModal;

    ExitProgram := FormControlPanel.NeedClose;

  finally
    FormControlPanel.Free;
  end;

  if ExitProgram
    then Close;
end;

procedure TFormMain.BitBtnEventHistoryClick(Sender: TObject);
begin
  Application.CreateForm(TFormEventLogs, FormEventLogs);
  FormEventLogs.ShowModal;
  FormEventLogs.Free;
end;

procedure TFormMain.BitBtnTemperatureRangesClick(Sender: TObject);
begin
  case InputPassword of
    pmWrong : ShowMessage('Wrong password');

    pmCorrect :
      begin
        Application.CreateForm(TFormTemperatureRanges, FormTemperatureRanges);
        FormTemperatureRanges.ShowModal;
        FormTemperatureRanges.Free;
      end;
  end;
end;

procedure TFormMain.ButtonApplyFloorAxisSettingsClick(Sender: TObject);
var
  MinYValue,
  MaxYValue : single;
begin
  if not TryStrToFloat(LabeledEditAxisMinYValue.Text, MinYValue, ApplicationFormatSettings)
    then
      begin
        ShowMessage('Incorrect input data');
        LabeledEditAxisMinYValue.SetFocus;
        LabeledEditAxisMinYValue.SelectAll;

        Exit;
      end;

  if not TryStrToFloat(LabeledEditAxisMaxYValue.Text, MaxYValue, ApplicationFormatSettings)
    then
      begin
        ShowMessage('Incorrect input data');
        LabeledEditAxisMaxYValue.SetFocus;
        LabeledEditAxisMaxYValue.SelectAll;

        Exit;
      end;


  if MinYValue >= MaxYValue
    then
      begin
        ShowMessage('Min value should be less than Max');

        LabeledEditAxisMinYValue.SetFocus;
        LabeledEditAxisMinYValue.SelectAll;
        Exit;
      end;

  ApplicationGraph.SetAxisRanges(MinYValue, MaxYValue);

  LabelAxisMinYValue.Caption := LabeledEditAxisMinYValue.Text;
  LabelAxisMaxYValue.Caption := LabeledEditAxisMaxYValue.Text;

  SaveSettings;

  PaintBox.Repaint;
end;

procedure TFormMain.ButtonDebugClick(Sender: TObject);
begin
  Application.CreateForm(TFormDebugPanel, FormDebugPanel);
  FormDebugPanel.ShowModal;
  FormDebugPanel.Free;
end;

procedure TFormMain.ComPortRxChar(Sender: TObject; Count: Integer);
var
  Buffer : TDynamicByteArray;

  IncomingMessage : TMIncomingComportMessage;
  SendingMessage : TMOutgoingComportMessage;
begin
  Inc(RecievedPackCount);

  SetLength(Buffer, Count);
  ComPort.Read(Buffer[0], Count);

  IncomingMessage := ApplicationComPortIncomingMessage; //Передаем значение по ссылке. Промежуточный объект используется для сокращения названия переменной

  if IncomingMessage.LoadFromBuffer(Buffer)
    then Inc(ProceedPackCount);

  SendingMessage := ApplicationComPortOutgoingMessages.SendingComPortMessage;

  if not Assigned(SendingMessage)
    then IncomingMessage.Error := imeNoSendingMessage;

  //Если сообщение полностью получено и во время получения не возникло ошибок
  if (IncomingMessage.State = imsRecieved)
    then ProcessIncomingMessage(IncomingMessage);

  if IncomingMessage.IsError
    then
      begin
        ProcessIncomingMessageErrors(IncomingMessage);//Если полученное сообщение содержит информацию об ошибках
        if Assigned(SendingMessage)
          then SendingMessage.Error := omeWrongAcknowledge;
      end;

  if (IncomingMessage.State = imsRecieved) or IncomingMessage.IsError
    then
      begin
        IncomingMessage.SaveToDataBase;
        IncomingMessage.Clear; //Есди сообщение получено или во время получения возникли ошибки
      end;
end;

procedure TFormMain.UpdateGraph();
var
  Series : TMSeries;

  i, j, k : integer;

  Pair: string;

  SectionNumber,
  ConveyorNumber : integer;

  Value : single;
begin
  ApplicationGraph.Series.Clear;

  for i := 0 to 5 do //Этажи
    begin
      for j := 0 to 3 do //Датчики
        begin
          Series := ApplicationGraph.AddSeries(i);

           case j of
            0 :
              begin
                Series.Color := clRed;
                Pair         := 'Top';
//                Pair := 'LeftTop';
              end;

            1 :
              begin
                Series.Color := clGreen;
                Pair         := 'Bottom';
//                Pair := 'LeftBottom';

              end;

            2 :
              begin
                Series.Color := clBlue;
//                Pair := 'RightTop';
                Pair         := 'Left';
              end;

            3 :
              begin
                Series.Color := clAqua;
//                Pair := 'RightBottom';
//
                Pair         := 'Right';
              end;
          end;

          for k := 0 to 9 do //Секции
            begin
              ConveyorNumber := 5 - i;
              SectionNumber  := k + 1;

              if i <> 5
                then Value := ApplicationTempBufferValues.GetAverage(SectionNumber, ConveyorNumber, Pair) //для всех этажей
                else Value := ApplicationTempBufferValues.GetAverage(SectionNumber, Pair); //для осредненного графика

              Series.AddPoint(Value, k);
            end;
        end;
    end;

  PaintBox.Repaint;
end;


function TFormMain.LoadSettings : boolean;
var
  MinYValueText,
  MaxYValueText,
  Port,
  BaudRate,
  DataBits,
  Parity,
  FlowControl : string;

  Conveyor : TMConveyor;
begin
  MinYValueText := FloatToStr(ApplicationProgramSettings.GraphSettings.AxisMinYValue, ApplicationFormatSettings);
  MaxYValueText := FloatToStr(ApplicationProgramSettings.GraphSettings.AxisMaxYValue, ApplicationFormatSettings);

  LabeledEditAxisMinYValue.Text := MinYValueText;
  LabeledEditAxisMaxYValue.Text := MaxYValueText;

  LabelAxisMinYValue.Caption := MinYValueText;
  LabelAxisMaxYValue.Caption := MaxYValueText;

  Conveyor     := ApplicationController.GetItem('1');
  case Conveyor.WorkMode of
    cwmOverlocking: TrackBarConveyor1.Position := 0;
    cwmWork:        TrackBarConveyor1.Position := 1;
  end;

  Conveyor     := ApplicationController.GetItem('2');
  case Conveyor.WorkMode of
    cwmOverlocking: TrackBarConveyor2.Position := 0;
    cwmWork:        TrackBarConveyor2.Position := 1;
  end;

  Conveyor     := ApplicationController.GetItem('3');
  case Conveyor.WorkMode of
    cwmOverlocking: TrackBarConveyor3.Position := 0;
    cwmWork:        TrackBarConveyor3.Position := 1;
  end;

  Conveyor     := ApplicationController.GetItem('4');
  case Conveyor.WorkMode of
    cwmOverlocking: TrackBarConveyor4.Position := 0;
    cwmWork:        TrackBarConveyor4.Position := 1;
  end;

  Conveyor     := ApplicationController.GetItem('5');
  case Conveyor.WorkMode of
    cwmOverlocking: TrackBarConveyor5.Position := 0;
    cwmWork:        TrackBarConveyor5.Position := 1;
  end;

  //Загрузка настроек COM-порта

  Port := ApplicationProgramSettings.UserSettings.Port;
  ComPort.Port := Port;

  BaudRate := ApplicationProgramSettings.UserSettings.BaudRate;
  ComPort.BaudRate := StrToBaudRate(BaudRate);

  DataBits := ApplicationProgramSettings.UserSettings.DataBits;
  ComPort.DataBits := StrToDataBits(DataBits);

  Parity := ApplicationProgramSettings.UserSettings.Parity;
  ComPort.Parity.Bits := StrToParity(Parity);

  FlowControl := ApplicationProgramSettings.UserSettings.FlowControl;
  ComPort.FlowControl.FlowControl:= StrToFlowControl(FlowControl);

  TimerCreateBoxMessages.Interval             := ApplicationProgramSettings.UserSettings.CreateBoxMessageInterval;
  TimerComPortSendMessages.Interval           := ApplicationProgramSettings.UserSettings.ComPortSendMessagesInterval;
  TimerCreateCheckSignaModelMessages.Interval := ApplicationProgramSettings.UserSettings.CreateCheckSignalModeMessagesInterval;
  TimerRefreshView.Interval                   := ApplicationProgramSettings.UserSettings.RefreshViewInterval;

  TimerCreateBoxMessages.Enabled             := ApplicationProgramSettings.UserSettings.EnableCreateBoxMessages;
  TimerComPortSendMessages.Enabled           := ApplicationProgramSettings.UserSettings.EnableComPortSendMessages;
  TimerCreateCheckSignaModelMessages.Enabled := ApplicationProgramSettings.UserSettings.EnableCreateCheckSignalModeMessagesInterval;

  Result := TRUE;
end;

function TFormMain.SaveSettings : boolean;
var
  MinYValue,
  MaxYValue : single;
begin
  MinYValue := StrToFloat(LabeledEditAxisMinYValue.Text, ApplicationFormatSettings);
  MaxYValue := StrToFloat(LabeledEditAxisMaxYValue.Text, ApplicationFormatSettings);

  ApplicationProgramSettings.GraphSettings.AxisMinYValue := MinYValue;
  ApplicationProgramSettings.GraphSettings.AxisMaxYValue := MaxYValue;

  Result := ApplicationProgramSettings.SaveToInifile;
end;

procedure TFormMain.UpdateStatusBar;
begin
  StatusBar.Panels[0].Text := 'Current time: '    + DateTimeToStr(Now, ApplicationFormatSettings);
  StatusBar.Panels[1].Text := 'Program uptime: '  + DateTimeToDHMSString(Now - FStartTime);
  StatusBar.Panels[2].Text := 'Program version: ' + ApplicationAttributes.GetApplicationVersionFull; 
end;

procedure TFormMain.UpdateSignalMode;
begin
  case ApplicationController.SignalMode of
    smNone     : BitBtnSirenDisable.Enabled := False;
    smEnabled  : BitBtnSirenDisable.Enabled := True;
    smDisabled : BitBtnSirenDisable.Enabled := False;
  end;
end;

function TFormMain.GraphMouseToGridCoord(AMouseCoord : TPoint; out AConveyorNumber, ASectionNumber : integer) : boolean;
var
  GridCoord : TPoint;
begin
  Result := False;

  GridCoord := ApplicationGraph.GetGridCoord(AMouseCoord);

  ASectionNumber  := GridCoord.X + 1;
  AConveyorNumber := 5 - GridCoord.Y;

  if (ASectionNumber > 10) or (ASectionNumber < 1)
    then Exit;

  if (AConveyorNumber > 5) or (AConveyorNumber < 1)
    then Exit;

  Result := TRUE;
end;

function TFormMain.ProcessIncomingMessage(IncomingMessage : TMIncomingComportMessage) : boolean;
var
  Box : TMBox;
  DeviceId : Byte;
  SendingMessage : TMOutgoingComportMessage;
begin
  Result := False;

  if IncomingMessage.IsError
    then Exit;

  SendingMessage := ApplicationComPortOutgoingMessages.SendingComPortMessage;

  if not Assigned(SendingMessage)
    then Exit;

  SendingMessage.IsTimeOutError; //Проверка таймаута

  if SendingMessage.IsError
    then
      begin
        IncomingMessage.Error := imeSendingMessage;

        Exit;
      end;

  case IncomingMessage.CommandId of
    $03, $04 : //Получение информации с датчиков и статус сигнализации
      begin
        case IncomingMessage.CommandId of
          $03 : //Статус сигнализации
            begin
              ApplicationController.CheckSignalModeLoadFromComPortMessage(IncomingMessage);
              UpdateSignalMode;
            end;

          $04 : //Данные с датчиков
            begin
              DeviceId := IncomingMessage.DeviceId;

              Box := ApplicationBoxes.GetItem(IntToStr(DeviceId));

              if not Assigned(Box)
                then Exit;

              if not Box.LoadFromComPortMessage(IncomingMessage)
                then Exit;
            end;
        end;

        if (SendingMessage.DeviceId = IncomingMessage.DeviceId)
          then
            begin
              SendingMessage.State := omsDelievered;
              SendingMessage.DelieveredTime := Now;
            end
          else IncomingMessage.Error := imeWrongDeviceId;
      end;

    $06 : //Эхо-ответ от сигналки. Используется для подтверждения того, что статус поменялся
      begin
        ApplicationController.SetSignalModeLoadFromComPortMessage(IncomingMessage);
        UpdateSignalMode;

        SendingMessage.State := omsDelievered;
        SendingMessage.DelieveredTime := Now;
      end;
  end;

  Inc(RecievedMessagesCount);

  Result := True;
end;

function TFormMain.ProcessIncomingMessageErrors(IncomingMessage : TMIncomingComportMessage) : boolean;
begin
  case IncomingMessage.Error of
    imeCRC :
      INC(IncomingErrorCRCCount);

    imeNoSendingMessage :
      Inc(IncomingErrorNoSendingMessageCount);

    imeSendingMessage : //ошибка в отправляемом сообщении на момент получения
      Inc(IncomingErrorSendingMessageCount);

    imeBufferOverflow :
      Inc(IncomingErrorBufferOverFlowCout);

    imeTimeoutEndPacket:
      Inc(IncomingErrorTimeOutEndPacketCount);

    imeWrongDeviceId:
      Inc(IncomingErrorWrongDeviceIdCount);
  end;

  Result := True;
end;

procedure TFormMain.ClearMessage(var SendingMessage : TMOutgoingComportMessage);
var
  message_uid : string;
begin
  //Очистка сообщения
  message_uid := SendingMessage.MessageUid;
  SendingMessage.SaveToDataBase;
  ApplicationComPortOutgoingMessages.DeleteItem(message_uid);
  ApplicationComPortOutgoingMessages.SendingComPortMessage := nil;
  SendingMessage := nil;
end;

function TFormMain.DeleteOutdatedTempValues() : boolean;
const
  CRecentDayCount = 15;
var
  TableRecord : TMTableRecord;
  QueryConstructor : TMQueryConstructor;

  ms_between,
  rows_affected : integer;

  before : TDateTime;
begin
  Result := False;

  //Удаляем значения температуры, старшие 15 дней, каждый день после 0:00 часов ночи.
  TableRecord := TMTableRecord.Create('TempValues');
  try
    QueryConstructor := TableRecord.QueryConstructor;

    if not Assigned(QueryConstructor)
      then Exit;

    QueryConstructor.AddCondition('TempValues', 'TempTime', ctLessEqual, Now - CRecentDayCount);

    before := Now;

    rows_affected := TableRecord.DeleteRecordsRowsAffected;

    ms_between := MilliSecondsBetween(before, Now);

    ApplicationEventLog.WriteLog(elDeleteOutdated, 'Deleted ' + IntToStr(rows_affected) +
                                                   ' records of outdated temp values in ' +
                                                   IntToStr(ms_between) + ' ms');
  finally
    TableRecord.Free;
  end;

  Result := True;
end;

function TFormMain.DeleteOutdatedComPortMessages() : boolean;
var
  TableRecord : TMTableRecord;

  ms_between : integer;

  before : TDateTime;
begin
  //Удаляем старые Com-port сообщения
  TableRecord := TMTableRecord.Create('Messages');
  try
    before := Now;

    TableRecord.DeleteRecordsAll;

    ms_between := MilliSecondsBetween(before, Now);

    ApplicationEventLog.WriteLog(elDeleteOutdated, 'Deleted all ComPort messages in ' +
                                                   IntToStr(ms_between) + ' ms');
  finally
    TableRecord.Free;
  end;

  Result := True;
end;


function TFormMain.BackupOutdatedTempValues() : boolean;
const
  CRecentDayCount = 1;
var
  ExportToCSV : TMExportToCSV;

  ms_between,
  rows_affected : integer;

  before,
  date_since,
  date_to : TDateTime;
begin
  //Удаляем значения температуры, старше 15 дней, каждый день после 0:00 часов ночи.
  ExportToCSV := TMExportToCSV.Create;
  try
    before := Now;

    date_since := Trunc(Now - CRecentDayCount);
    date_to    := Trunc(Now);

    rows_affected := ExportToCSV.SaveToCSVFile(date_since, date_to);

    ms_between := MilliSecondsBetween(before, Now);

    ApplicationEventLog.WriteLog(elBackupOutdated, 'Saved ' + IntToStr(rows_affected) +
                                                   ' records of outdated temp values in ' +
                                                   IntToStr(ms_between) + ' ms');
  finally
    ExportToCSV.Free;
  end;

  Result := True;
end;


end.
