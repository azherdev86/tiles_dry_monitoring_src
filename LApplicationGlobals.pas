unit LApplicationGlobals;

interface

uses CDataBaseStructure, ZConnection, ZDataset, SysUtils, CController,
     Controls, StdCtrls, CGraph, CBoxes, CIncomingComPortMessage,
     COutgoingComPortMessage, CProgramSettings, CTempValuesBuffer;

procedure CreateGlobals;
procedure UpdateGlobals;
procedure DestroyGlobals;
function  ConnectToDataBase(var OutMessage : string) : boolean;
procedure UpdateFormatSettings();

var
  ApplicationDataBaseStructure       : TMDatabaseStructure; //Структура данных
  ApplicationDBConnection            : TZConnection;
  ApplicationFormatSettings          : TFormatSettings;
  ApplicationGraph                   : TMGraph;
  ApplicationBoxes                   : TMBoxesList;
  ApplicationComPortOutgoingMessages : TMOutgoingComportMessagesList;
  ApplicationComPortIncomingMessage  : TMIncomingComportMessage;
  ApplicationProgramSettings         : TMProgramSettings;
  ApplicationTempBufferValues        : TMTempBufferValuesList; //Последние 200 значений
  ApplicationController              : TMController;

  GLOBAL_DATABASE_TYPE : TMDatabaseType = dtFireBird;

const
//Для подключения к MySQL серверу
  MySQL_ZeosProtocol    = 'mysql';
  MySQL_ServerHostName  = 'localhost';
  MySQL_DBUserName      = 'root';
  MySQL_DBPassWord      = '1';
  MySQL_DBName          = 'mt5a';


//Для подключения к FireBird серверу
  FireBird_DBAliasName    = 'maintest5a.fdb';
  FireBird_ZeosProtocol   = 'firebird-2.5';
  FireBird_HostName       = '';
  FireBird_Catalog        = '';
  FireBird_LibFileName    = 'fbembed.dll';
  FireBird_DBFilePath     = 'database\TempSensors.FDB';
  FireBird_DBUserName     = 'SYSDBA';
  FireBird_DBPassWord     = 'MASTERKEY';
  FireBird_ClientCodePage = 'WIN1251';

implementation

uses Dialogs, Forms, Classes, Windows, StrUtils, Variants,
     FMain;

procedure CreateGlobals;
begin
  ApplicationDBConnection                   := TZConnection.Create(nil);
  ApplicationDataBaseStructure              := TMDatabaseStructure.Create(ApplicationDBConnection);
  ApplicationDataBaseStructure.DatabaseType := GLOBAL_DATABASE_TYPE;
  ApplicationTempBufferValues               := TMTempBufferValuesList.Create;
  ApplicationGraph                          := TMGraph.Create;
  ApplicationBoxes                          := TMBoxesList.Create;
  ApplicationComPortOutgoingMessages        := TMOutgoingComportMessagesList.Create;
  ApplicationComPortIncomingMessage         := TMIncomingComportMessage.Create;
  ApplicationProgramSettings                := TMProgramSettings.Create;
  ApplicationController                     := TMController.Create;
end;

procedure UpdateGlobals;
var
  TextMessage : string;
begin
  ApplicationProgramSettings.LoadFromInifile;
  ApplicationGraph.LoadSettings;

  TextMessage := 'Database connection Error';
  UpdateFormatSettings();

  if not ConnectToDataBase(TextMessage)
    then
      begin
        ShowMessage(TextMessage);
        Exit;
      end;

  if not ApplicationDataBaseStructure.LoadStructure
    then Exit;

  ApplicationBoxes.Init;
  ApplicationController.Init;
end;


procedure DestroyGlobals;
begin
  ApplicationBoxes.Free;
  ApplicationComPortOutgoingMessages.Free;
  ApplicationComPortIncomingMessage.Free;
  ApplicationTempBufferValues.Free;
  ApplicationDataBaseStructure.Free;
  ApplicationDBConnection.Free;
  ApplicationGraph.Free;
  ApplicationController.Free;

  ApplicationProgramSettings.SaveToInifile;
  ApplicationProgramSettings.Free;
end;


function ConnectToDataBase(var OutMessage : string) : boolean;
begin
  Result := false;

  if not Assigned(ApplicationDBConnection)
    then
      begin
        OutMessage := 'Object ''ApplicationDBConnection'' has not been created';
        Exit;
      end;

  case ApplicationDataBaseStructure.DatabaseType of
    dtMySQL :
      begin
        ApplicationDBConnection.Protocol    := MySQL_ZeosProtocol;
        ApplicationDBConnection.HostName    := MySQL_ServerHostName;
        ApplicationDBConnection.Database    := MySQL_DBName;
        ApplicationDBConnection.LoginPrompt := FALSE;
        ApplicationDBConnection.User        := MySQL_DBUserName;
        ApplicationDBConnection.Password    := MySQL_DBPassWord;
      end;
    dtFireBird :
      begin
        ApplicationDBConnection.Protocol        := FireBird_ZeosProtocol;
        ApplicationDBConnection.HostName        := FireBird_HostName;
        ApplicationDBConnection.Catalog         := FireBird_Catalog;
        ApplicationDBConnection.LibraryLocation := FireBird_LibFileName;
        ApplicationDBConnection.LoginPrompt     := FALSE;
        ApplicationDBConnection.User            := FireBird_DBUserName;
        ApplicationDBConnection.Password        := FireBird_DBPassWord;
        ApplicationDBConnection.Database        := FireBird_DBFilePath;
       // ApplicationDBConnection.
      end;
  end;

  try
    OutMessage := 'Не удалось подключиться к базе данных.';
    ApplicationDBConnection.Connected := TRUE;
    if not ApplicationDBConnection.Connected
      then
        begin
          exit;
        end
      else
        begin
          OutMessage := '';
          Result := TRUE;
        end;
  except
    on E : Exception do
      OutMessage := OutMessage + #13#10 +
                    E.Message;
  end;
end;

procedure UpdateFormatSettings();
begin
  GetLocaleFormatSettings(0, ApplicationFormatSettings);
  ApplicationFormatSettings.DateSeparator := '.';
  ApplicationFormatSettings.TimeSeparator := ':';
  ApplicationFormatSettings.DecimalSeparator := ',';
  ApplicationFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  ApplicationFormatSettings.LongTimeFormat := 'hh:mm:ss';
end;

initialization
  CreateGlobals;
  UpdateGlobals;

finalization
  DestroyGlobals;

end.
