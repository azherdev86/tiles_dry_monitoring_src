unit LUtils;

interface

function TimeToFirebirdString(const Date: TDateTime): string;
function DateToFirebirdString(const Date: TDateTime): string;
function DateTimeToFirebirdString(const Date: TDateTime): string;
function DateTimeToDHMSString(DateTime : TDateTime) : string;


implementation

uses SysUtils;

function DateTimeToDHMSString(DateTime : TDateTime) : string;
var
  ADay,
  AHour,
  AMinute,
  ASecond,
  AMilliSecond: Word;
begin
  Result := '';

  DecodeTime(DateTime, AHour, AMinute, ASecond, AMilliSecond);
  ADay  := Trunc(DateTime);

  if ADay > 0
    then Result := Result + IntToStr(ADay) + 'd. ';

  if (AHour > 0) or (ADay > 0)
    then Result := Result + IntToStr(AHour) + 'h. ';

  if (AHour > 0) or (ADay > 0) or (AMinute > 0)
    then Result := Result + IntToStr(AMinute) + 'm. ';

  Result := Result + IntToStr(ASecond) + 's.';
end;


function TimeToFirebirdString(const Date: TDateTime): string;
var
  Hour,
  Minute,
  Second,
  MSeconds : word;
  HourStr,
  MinuteStr,
  SecondStr : string;
  Delimiter : string;
begin
  Delimiter := ':';

  DecodeTime(Date, Hour, Minute, Second, MSeconds);

  if Hour < 10
    then HourStr := '0' + IntToStr(Hour)
    else HourStr := IntToStr(Hour);

  if Minute < 10
    then MinuteStr := '0' + IntToStr(Minute)
    else MinuteStr := IntToStr(Minute);

  if Second < 10
    then SecondStr := '0' + IntToStr(Second)
    else SecondStr := IntToStr(Second);

  Result := HourStr + Delimiter + MinuteStr + Delimiter + SecondStr;
end;


function DateToFirebirdString(const Date: TDateTime): string;
var
  Year,
  Day,
  Month : word;
  YearStr,
  DayStr,
  MonthStr : string;
  Delimiter : string;
begin
  Delimiter := '.';

  DecodeDate(Date, Year, Month, Day);

  YearStr := IntToStr(Year);
  if Month < 10
    then MonthStr := '0' + IntToStr(Month)
    else MonthStr := IntToStr(Month);

  if Day < 10
    then DayStr := '0' + IntToStr(Day)
    else DayStr := IntToStr(Day);

  Result := YearStr + Delimiter + MonthStr + Delimiter + DayStr;
end;


function DateTimeToFirebirdString(const Date: TDateTime): string;
begin
  Result := DateToFirebirdString(Date) + ' ' +
            TimeToFirebirdString(Date);
end;


end.
