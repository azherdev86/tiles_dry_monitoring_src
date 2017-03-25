unit LFileLogging;

interface

type TypeFileLogType = (fltNone,
                        fltIncomingMessages,
                        fltOutgoingMessages);

procedure WriteLog(AMessage : string; AType : TypeFileLogType);

implementation

uses SysUtils, DateUtils, LUtils, LApplicationGlobals, Windows;


function GetFileName() : string;
var
  year,
  month,
  day : word;

  year_str,
  month_str,
  day_str : string;
begin
  DecodeDate(Now, year, month, day);

  year_str  := IntToStr(year);
  month_str := IntToStr(month);
  day_str   := IntToStr(day);

  if month < 10
    then month_str := '0' + month_str;

  if day < 10
    then day_str := '0' + day_str;

  Result := year_str + month_str + day_str + '.txt';
end;


procedure WriteLog(AMessage : string; AType : TypeFileLogType);
const
  CFileLogDirectory = 'logs\';
var
  log_file : TextFile;

  path_year,
  path_month,
  path,
  filename : string;
  year,
  month : integer;

  hndl : integer;
begin
  if not DirectoryExists(CFileLogDirectory)
    then CreateDir(CFileLogDirectory);

  case AType of
    fltIncomingMessages: path := CFileLogDirectory + 'in\';
    fltOutgoingMessages: path := CFileLogDirectory + 'out\';
  end;

  if not DirectoryExists(path)
    then CreateDir(path);

  year :=  DecodeYear(Now);
  month := DecodeMonth(Now);

  path_year  := IntToStr(year);
  path_month := IntToStr(month);

  if month < 10
    then path_month := '0' + path_month;

  path := path + path_year + '\';

  if not DirectoryExists(path)
    then CreateDir(path);

  path := path + path_month + '\';

  if not DirectoryExists(path)
    then CreateDir(path);

  filename := GetFileName;
  filename := path + filename;

  if not FileExists(filename)
    then
      begin
        hndl := FileCreate(filename);
        FileClose(hndl);
      end;
      
  try
    Assign(log_file, filename);

    if FileExists(filename)
      then
        begin
          Append(log_file);

          Writeln(log_file, '[' + DateTimeToStr(Now,ApplicationFormatSettings) + ']: ' + AMessage);
        end;

  finally
    CloseFile(log_file);
  end;
end;

end.
