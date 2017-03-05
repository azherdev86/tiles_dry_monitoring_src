unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CPort, StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs, Chart, Buttons,
  ComCtrls;

type
  TFormMain = class(TForm)
    PaintBox: TPaintBox;
    TimerCreateComPortMessages: TTimer;
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
    BitBtnKeyBoard: TBitBtn;
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
    TimerUpdateInfo: TTimer;
    ImageGraphLegend: TImage;
    TrackBarConveyor1: TTrackBar;
    LabelConveyor1Overlocking: TLabel;
    LabelConveyor1Work: TLabel;
    TrackBarConveyor2: TTrackBar;
    Label17: TLabel;
    Label18: TLabel;
    TrackBarConveyor3: TTrackBar;
    Label20: TLabel;
    Label21: TLabel;
    TrackBarConveyor4: TTrackBar;
    Label22: TLabel;
    Label23: TLabel;
    TrackBarConveyor5: TTrackBar;
    Label24: TLabel;
    Label26: TLabel;
    TrackBarAllConveyors: TTrackBar;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    BitBtnChangePassword: TBitBtn;
    BitBtbExportToCSV: TBitBtn;
    ButtonDebug: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BitBtnKeyBoardClick(Sender: TObject);
    procedure BitBtnTemperatureRangesClick(Sender: TObject);
    procedure BitBtnEventHistoryClick(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure TimerCreateComPortMessagesTimer(Sender: TObject);
    procedure TimerComPortSendMessagesTimer(Sender: TObject);
    procedure TimerUpdateInfoTimer(Sender: TObject);
    procedure ComPortException(Sender: TObject; TComException: TComExceptions;
      ComportMessage: string; WinError: Int64; WinMessage: string);
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
    procedure BitBtnChangePasswordClick(Sender: TObject);
    procedure BitBtbExportToCSVClick(Sender: TObject);
    procedure ButtonDebugClick(Sender: TObject);
  private
    { Private declarations }
    FStartTime : double;
    FGraphMouseCoord : TPoint;

    procedure DrawSeries();
    function LoadSettings : boolean;
    function SaveSettings : boolean;
    procedure UpdateStatusBar;

    function GraphMouseToGridCoord(AMouseCoord : TPoint; out AConveyorNumber, ASectionNumber : integer) : boolean;

  public
    { Public declarations }
    SentMessagesCount           : integer;
    ReSentMessagesCount         : integer;
    RecievedMessagesCount       : integer;
    RecievedPackCount           : integer;
    ProceedPackCount            : integer;
    ErrorCRCCount               : integer;
    ErrorNoSendingMessageCount  : integer;
    ErrorBufferOverFlowCout     : integer;
    ErrorTimeOutEndPacketCount  : integer;
    ErrorWrongCmdOrDeviceId     : integer;
    ErrorTimeOutSendPacketCount : integer;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses LApplicationGlobals, CGraph, ShellAPI, FTemperatureRanges, FEventLogs,
     FGraphHistory, CBoxes, CBasicComPortMessage, CIncomingComPortMessage,
     COutgoingComPortMessage, DateUtils, CTableRecords, ZDataset, CTempValuesBuffer,
     CController, FInputPassword, FChangePassword, FExportToCSV, CEventLog, LUtils,
  FDebugPanel;


procedure FreeAndNilMessage(out ComPortMessage : TMOutgoingComportMessage);
begin
  ApplicationComPortOutgoingMessages.DeleteItem(ComPortMessage.MessageUid);
  ApplicationComPortOutgoingMessages.SendingComPortMessage := nil;
  ComPortMessage := nil;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FGraphMouseCoord := Point(0, 0);

  FormMain.WindowState := wsMaximized;
  FormMain.DoubleBuffered := TRUE;
  DrawSeries;

  FStartTime := Now;

  SentMessagesCount           := 0;
  ReSentMessagesCount         := 0;
  RecievedMessagesCount       := 0;
  RecievedPackCount           := 0;
  ProceedPackCount            := 0;
  ErrorCRCCount               := 0;
  ErrorNoSendingMessageCount  := 0;
  ErrorBufferOverFlowCout     := 0;
  ErrorTimeOutEndPacketCount  := 0;
  ErrorWrongCmdOrDeviceId     := 0;
  ErrorTimeOutSendPacketCount := 0;

  LoadSettings;

  ComPort.Connected := TRUE;

  UpdateStatusBar;
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

procedure TFormMain.TimerCreateComPortMessagesTimer(Sender: TObject);
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
    then Exit;


//  for j := 0 to 8 do
//  begin

  for i := 0 to count - 1 do
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

procedure TFormMain.TimerUpdateInfoTimer(Sender: TObject);
begin
  ApplicationController.CheckTemperatureRanges;

  DrawSeries;

  UpdateStatusBar;
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
  OutgoingMessage : TMOutgoingComportMessage;
  MessageBytes : TDynamicByteArray;

  SentTime : TDateTime;
  ms : integer;

  Box : TMBox;

  DeviceId : integer;
begin
  //!!!!!!!!!!!!!ОСТАВЛЯЮ НА ПОТОМ, Т.К. МОЖНО ЗАКОПАТЬСЯ!!!!!!!!!!!!!!!!!
  //Нужно расписать сюда повторную отправку сообщений в случае
  //Если при получении были ошибки. Сейчас эти ошибки фиксируются
  //Но повторная отправка не происходит

  if ApplicationComPortOutgoingMessages.GetCount = 0
    then Exit; //Если нечего отсылать - выходим

  OutgoingMessage := ApplicationComPortOutgoingMessages.SendingComPortMessage;

  if Assigned(OutgoingMessage) and (not ApplicationComPortOutgoingMessages.LastMessageError)
  //Если сообщение уже в отправке, а ошибок не зафиксировано, то проверям ТаймАут
    then
      begin
        try
          SentTime := OutgoingMessage.SentTime;
        except
          SentTime := 0;
          //одновременный доступ к объекту ComPortMessage из отправки сообщений
          //и из получения сообщений
        end;
          ms := MilliSecondsBetween(SentTime, Now);

          if ms < TIMEOUT_SEND_MESSAGE
            then Exit //Если Таймаут еще не прошел, то выходим
            else
              begin //Если ТаймАут прошел, то помечаем сообщение соответствующим образом
                OutgoingMessage.Error := omeTimeout;

                FreeAndNilMessage(OutgoingMessage);
              end;
      end;

  if ApplicationComPortOutgoingMessages.LastMessageError
  //Если при обработке предыдущих сообщений возникли ошибки, то
  //нужно эту отправку повторить (в дальнейшем нужен счетчик)
    then
      begin
        DeviceId := ApplicationComPortOutgoingMessages.LastSendingDeviceId;

        OutgoingMessage := TMOutgoingComportMessage.Create;

        Box := ApplicationBoxes.GetItem(IntToStr(DeviceId));

        if not Assigned(Box)
          then Exit;

        if not Box.SaveToComPortMessage(OutgoingMessage)
          then OutgoingMessage.Free;

        if Assigned(OutgoingMessage)
          then ApplicationComPortOutgoingMessages.AddItem(OutgoingMessage);

      end;

  if not Assigned(OutgoingMessage) //Если сообщение не создано ранее, т.е. не идет повторная отправка
  //то мы берем сообщение из очереди в соответствии со временем создания и приоритетом
    then OutgoingMessage := ApplicationComPortOutgoingMessages.GetMessageToSend;

  if not Assigned(OutgoingMessage)
    then Exit;

  OutgoingMessage.GenerateMessage(MessageBytes);

  if ComPort.Write(MessageBytes[0], Length(MessageBytes)) > 0
    then
      begin
        OutgoingMessage.State           := omsWaitResponse;
        OutgoingMessage.SentTime        := Now;
//        OutgoingMessage.SentTimeCounter := OutgoingMessage.SentTimeCounter + 1;

        ApplicationComPortOutgoingMessages.SendingComPortMessage := OutgoingMessage;

//        WriteLog('Сообщение отправлено:' + OutgoingMessage.MessageUid + '. Коробка ' + IntToStr(OutgoingMessage.DebugDeviceId));

        Inc(SentMessagesCount);

        if OutgoingMessage.SentTimeCounter > 1
          then Inc(ReSentMessagesCount);
      end;
end;


procedure TFormMain.BitBtnKeyBoardClick(Sender: TObject);
begin
  ShellExecute(0,nil,'osk.exe',nil,nil,SW_SHOW);
end;

procedure TFormMain.BitBtbExportToCSVClick(Sender: TObject);
begin
  Application.CreateForm(TFormExportToCSV, FormExportToCSV);
  FormExportToCSV.ShowModal;
  FormExportToCSV.Free;
end;

procedure TFormMain.BitBtnChangePasswordClick(Sender: TObject);
begin
  Application.CreateForm(TFormChangePassword, FormChangePassword);
  FormChangePassword.ShowModal;
  FormChangePassword.Free;
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

procedure TFormMain.ComPortException(Sender: TObject;
  TComException: TComExceptions; ComportMessage: string; WinError: Int64;
  WinMessage: string);
begin
//  Inc(Excep);
end;

procedure TFormMain.ComPortRxChar(Sender: TObject; Count: Integer);
var
  DeviceId : Byte;

  Buffer : TDynamicByteArray;

  Box : TMBox;
  SendingComPortMessage : TMOutgoingComportMessage;
begin
  Inc(RecievedPackCount);
  SetLength(Buffer, Count);
  ComPort.Read(Buffer[0], Count);

  if ApplicationComPortIncomingMessage.LoadFromBuffer(Buffer)
    then Inc(ProceedPackCount);

  SendingComPortMessage := ApplicationComPortOutgoingMessages.SendingComPortMessage;

//  ApplicationComPortIncomingMessage.DebugDeviceId := SendingComPortMessage.DebugDeviceId;

  if not Assigned(SendingComPortMessage)
    then ApplicationComPortIncomingMessage.Error := imeNoSendingMessage;

  if (ApplicationComPortIncomingMessage.State = imsRecieved) and
     (not ApplicationComPortIncomingMessage.IsError)
    then
      begin
        ApplicationComPortOutgoingMessages.LastMessageError := False;

        Inc(RecievedMessagesCount);

        DeviceId := ApplicationComPortIncomingMessage.DeviceId;
//        DeviceId := ApplicationComPortIncomingMessage.DebugDeviceId;

        Box := ApplicationBoxes.GetItem(IntToStr(DeviceId));

        if not Assigned(Box)
          then Exit;

        if not Box.LoadFromComPortMessage(ApplicationComPortIncomingMessage)
          then Exit;

        if (SendingComPortMessage.DeviceId = ApplicationComPortIncomingMessage.DeviceId) and
           (ApplicationComPortIncomingMessage.CommandId = $04) and
           (SendingComPortMessage.CommandId = $04)
          then
            begin
              SendingComPortMessage.State := omsDelievered;
              SendingComPortMessage.DelieveredTime := Now;

//              WriteLog('Данные сохранены. Сообщение доставлено: ' + SendingComPortMessage.MessageUid + '. Байт получено: ' + IntToStr(ApplicationComPortIncomingMessage.IncomingByteIndex));
            end
          else ApplicationComPortIncomingMessage.Error := imeWrongCmdOrDeviceId;

      end;

    if ApplicationComPortIncomingMessage.IsError
    then
      begin
        ApplicationComPortOutgoingMessages.LastMessageError := TRUE;

        case ApplicationComPortIncomingMessage.Error of
          imeCRC :
            INC(ErrorCRCCount);

          imeNoSendingMessage :
            Inc(ErrorNoSendingMessageCount);

          imeBufferOverflow :
            Inc(ErrorBufferOverFlowCout);

          imeTimeoutEndPacket:
            Inc(ErrorTimeOutEndPacketCount);

          imeWrongCmdOrDeviceId:
            Inc(ErrorWrongCmdOrDeviceId);
        end;
      end;


  if (ApplicationComPortIncomingMessage.State = imsRecieved) or
      ApplicationComPortIncomingMessage.IsError
    then
      begin
        if Assigned(SendingComPortMessage)
          then ApplicationComPortOutgoingMessages.DeleteItem(SendingComPortMessage.MessageUid);

        ApplicationComPortOutgoingMessages.SendingComPortMessage := nil;
        SendingComPortMessage := nil;

        ApplicationComPortIncomingMessage.Clear;
      end;
end;

procedure TFormMain.DrawSeries();
var
  Series : TMSeries;

  i, j, k : integer;

  Pair: string;

  SectionNumber,
  ConveyorNumber : integer;

  TempValueBuffer : TMTempBufferValue;

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
              ConveyorNumber := i + 1;
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
  StatusBar.Panels[2].Text := 'Program version: ' + '1.0.0';
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

end.
