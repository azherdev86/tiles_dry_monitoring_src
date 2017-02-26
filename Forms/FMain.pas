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
    BitBtn1: TBitBtn;
    GroupBoxFloorAxisSettings: TGroupBox;
    LabeledEditAxisMinYValue: TLabeledEdit;
    LabeledEditAxisMaxYValue: TLabeledEdit;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ButtonApplyFloorAxisSettings: TButton;
    Button2: TButton;
    LabeledEdit3: TLabeledEdit;
    Button3: TButton;
    LabeledEdit4: TLabeledEdit;
    Button4: TButton;
    LabeledEdit5: TLabeledEdit;
    Button5: TButton;
    LabeledEdit6: TLabeledEdit;
    Button6: TButton;
    LabeledEdit7: TLabeledEdit;
    Label19: TLabel;
    StatusBar1: TStatusBar;
    ComPort: TComPort;
    MemoLogs: TMemo;
    TimerComPortSendMessages: TTimer;
    Button7: TButton;
    Button8: TButton;
    TimerUpdateInfo: TTimer;
    MemoInfo: TMemo;
    ImageGraphLegend: TImage;
    TrackBar1: TTrackBar;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure TimerCreateComPortMessagesTimer(Sender: TObject);
    procedure TimerComPortSendMessagesTimer(Sender: TObject);
    procedure TimerUpdateInfoTimer(Sender: TObject);
    procedure ComPortException(Sender: TObject; TComException: TComExceptions;
      ComportMessage: string; WinError: Int64; WinMessage: string);
    procedure Button8Click(Sender: TObject);
    procedure ButtonApplyFloorAxisSettingsClick(Sender: TObject);
    procedure LabeledEditAxisMinYValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEditAxisMaxYValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    StartTime : double;
    FGraphMouseCoord : TPoint;

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

    procedure WriteLog(AMessage : string);

    procedure DrawSeries();
    function LoadSettings : boolean;
    function SaveSettings : boolean;

    function GraphMouseToGridCoord(AMouseCoord : TPoint; out AConveyorNumber, ASectionNumber : integer) : boolean;

  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses LApplicationGlobals, CGraph, ShellAPI, FTemperatureRanges, FEventHistory,
     FGraphHistory, CBoxes, CBasicComPortMessage, CIncomingComPortMessage,
     COutgoingComPortMessage, DateUtils, CTableRecords, ZDataset, CTempValuesBuffer;


procedure FreeAndNilMessage(out ComPortMessage : TMOutgoingComportMessage);
begin
  ApplicationComPortOutgoingMessages.DeleteItem(ComPortMessage.MessageUid);
  ApplicationComPortOutgoingMessages.SendingComPortMessage := nil;
  ComPortMessage := nil;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FGraphMouseCoord := Point(0, 0);

  MemoLogs.Visible := False;
  MemoInfo.Visible := False;
  FormMain.WindowState := wsMaximized;
  FormMain.DoubleBuffered := TRUE;
  DrawSeries;
  TimerCreateComPortMessages.Enabled := TRUE;

  StartTime := Now;
  ComPort.Connected := TRUE;

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

procedure TFormMain.WriteLog(AMessage : string);
begin
  MemoLogs.Lines.Add('[' + DateTimeToStr(Now, ApplicationFormatSettings) + ']: ' + AMessage);
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
end;

procedure TFormMain.TimerUpdateInfoTimer(Sender: TObject);
var
  QueueCount : integer;
begin
  QueueCount := ApplicationComPortOutgoingMessages.GetCount;

  MemoInfo.Clear;

  MemoInfo.Lines.Add('Сообщений в очереди:' + #9 + IntToStr(QueueCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Отправлено сообщений:' + #9 + IntToStr(SentMessagesCount));
  MemoInfo.Lines.Add('Получено сообщений:' + #9 + IntToStr(RecievedMessagesCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Получено пачек:' + #9 + #9 + IntToStr(RecievedPackCount));
  MemoInfo.Lines.Add('Обработано пачек:' + #9 + IntToStr(ProceedPackCount));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('Повторных отправок:' + #9 + #9 + IntToStr(ReSentMessagesCount));
  MemoInfo.Lines.Add('ErrorCRCCount:' + #9 + #9 + #9 + IntToStr(ErrorCRCCount));
  MemoInfo.Lines.Add('ErrorNoSendingMessageCount:' + #9 + IntToStr(ErrorNoSendingMessageCount));
  MemoInfo.Lines.Add('ErrorBufferOverFlowCout:' + #9 + #9 + IntToStr(ErrorBufferOverFlowCout));
  MemoInfo.Lines.Add('ErrorTimeOutEndPacketCount:' + #9 + IntToStr(ErrorTimeOutEndPacketCount));
  MemoInfo.Lines.Add('ErrorWrongCmdOrDeviceId:' + #9 + #9 + IntToStr(ErrorWrongCmdOrDeviceId));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('ErrorTimeOutSendPacketCount:' + #9 + IntToStr(ErrorTimeOutSendPacketCount));

  ApplicationController.CheckTemperatureRanges;

  DrawSeries;
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
        if (DeviceId > $00) and (DeviceId <= $14)
          then
            begin
              OutgoingMessage := TMOutgoingComportMessage.Create;

              Box := ApplicationBoxes.GetItem(IntToStr(DeviceId));

              if not Assigned(Box)
                then Exit;

              if not Box.SaveToComPortMessage(OutgoingMessage)
                then OutgoingMessage.Free;

              if Assigned(OutgoingMessage)
                then ApplicationComPortOutgoingMessages.AddItem(OutgoingMessage);
            end;
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

        WriteLog('Сообщение отправлено:' + OutgoingMessage.MessageUid + '. Коробка ' + IntToStr(OutgoingMessage.DebugDeviceId));

        Inc(SentMessagesCount);

        if OutgoingMessage.SentTimeCounter > 1
          then Inc(ReSentMessagesCount);
      end;
end;


procedure TFormMain.BitBtn1Click(Sender: TObject);
begin
  ShellExecute(0,nil,'osk.exe',nil,nil,SW_SHOW);
end;

procedure TFormMain.BitBtn2Click(Sender: TObject);
begin
  Application.CreateForm(TFormEventHistory, FormEventHistory);
  FormEventHistory.ShowModal;
  FormEventHistory.Free;
end;

procedure TFormMain.BitBtn3Click(Sender: TObject);
var
  pass : string;
begin
  if InputQuery('Password required', 'Input admin password to proceed', pass)
    then
      begin
        if pass <> 'admin'
          then
            begin
              ShowMessageUser('Wrong password');
              BitBtn3.Click;
            end
          else
            begin
              Application.CreateForm(TFormTemperatureRanges, FormTemperatureRanges);
              FormTemperatureRanges.ShowModal;
              FormTemperatureRanges.Free;
            end;
      end


//  Input
end;

procedure TFormMain.Button1Click(Sender: TObject);
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

procedure TFormMain.Button2Click(Sender: TObject);
begin
  Button2.Tag := Button2.Tag + 1;

  if (Button2.Tag mod 2) = 0
    then LabeledEdit3.Text := 'overlocking'
    else LabeledEdit3.Text := 'operation';
end;

procedure TFormMain.Button3Click(Sender: TObject);
begin
  Button3.Tag := Button3.Tag + 1;

  if (Button3.Tag mod 2) = 0
    then LabeledEdit4.Text := 'overlocking'
    else LabeledEdit4.Text := 'operation';
end;

procedure TFormMain.Button4Click(Sender: TObject);
begin
  Button4.Tag := Button4.Tag + 1;

  if (Button4.Tag mod 2) = 0
    then LabeledEdit5.Text := 'overlocking'
    else LabeledEdit5.Text := 'operation';

end;

procedure TFormMain.Button5Click(Sender: TObject);
begin
  Button5.Tag := Button5.Tag + 1;

  if (Button5.Tag mod 2) = 0
    then LabeledEdit6.Text := 'overlocking'
    else LabeledEdit6.Text := 'operation';
end;

procedure TFormMain.Button7Click(Sender: TObject);
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

  TableRecord := TMTableRecord.Create('Sensors');
  try
    for i := 0 to 19 do //Коробки
      begin
        for j := 0 to 4 do //Этажи
          begin
            BoxNumber      := i + 1;
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

procedure TFormMain.Button8Click(Sender: TObject);
begin
  TimerComPortSendMessages.Enabled := not TimerComPortSendMessages.Enabled;
  TimerCreateComPortMessages.Enabled := not TimerCreateComPortMessages.Enabled;
end;

procedure TFormMain.ButtonApplyFloorAxisSettingsClick(Sender: TObject);
var
  MinYValue,
  MaxYValue : single;
begin
  MinYValue := StrToFloat(LabeledEditAxisMinYValue.Text, ApplicationFormatSettings);
  MaxYValue := StrToFloat(LabeledEditAxisMaxYValue.Text, ApplicationFormatSettings);

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

  ApplicationComPortIncomingMessage.DebugDeviceId := SendingComPortMessage.DebugDeviceId;

  if not Assigned(SendingComPortMessage)
    then ApplicationComPortIncomingMessage.Error := imeNoSendingMessage;

  if (ApplicationComPortIncomingMessage.State = imsRecieved) and
     (not ApplicationComPortIncomingMessage.IsError)
    then
      begin
        ApplicationComPortOutgoingMessages.LastMessageError := False;

        Inc(RecievedMessagesCount);

//        DeviceId := ApplicationComPortIncomingMessage.DeviceId;
        DeviceId := ApplicationComPortIncomingMessage.DebugDeviceId;

        Box := ApplicationBoxes.GetItem(IntToStr(DeviceId));

        if not Assigned(Box)
          then Exit;

        if not Box.LoadFromComPortMessage(ApplicationComPortIncomingMessage)
          then Exit;

        if (SendingComPortMessage.DebugDeviceId = ApplicationComPortIncomingMessage.DebugDeviceId) and
           (ApplicationComPortIncomingMessage.CommandId = $03) and
           (SendingComPortMessage.CommandId = $03)
          then
            begin
              SendingComPortMessage.State := omsDelievered;
              SendingComPortMessage.DelieveredTime := Now;

              WriteLog('Данные сохранены. Сообщение доставлено: ' + SendingComPortMessage.MessageUid + '. Байт получено: ' + IntToStr(ApplicationComPortIncomingMessage.IncomingByteIndex));
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
              end;

            1 :
              begin
                Series.Color := clGreen;
                Pair         := 'Bottom';
              end;

            2 :
              begin
                Series.Color := clBlue;
                Pair         := 'Left';
              end;

            3 :
              begin
                Series.Color := clAqua;
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
  MaxYValueText : string;
begin
  MinYValueText := FloatToStr(ApplicationProgramSettings.GraphSettings.AxisMinYValue, ApplicationFormatSettings);
  MaxYValueText := FloatToStr(ApplicationProgramSettings.GraphSettings.AxisMaxYValue, ApplicationFormatSettings);

  LabeledEditAxisMinYValue.Text := MinYValueText;
  LabeledEditAxisMaxYValue.Text := MaxYValueText;

  LabelAxisMinYValue.Caption := MinYValueText;
  LabelAxisMaxYValue.Caption := MaxYValueText;

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
