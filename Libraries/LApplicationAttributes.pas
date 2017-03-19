unit LApplicationAttributes;


interface
uses Classes;


type
  TAApplicationAttributes = class
    constructor Create;
    destructor Destroy; override;
  private
    AttribsList : TStringList;
    procedure Initailize;
    procedure LoadApplicationDateTime;
    procedure LoadApplicationAttributes;
  public
    ApplicationFilePath : string;
    ApplicationVersion : string;
    ApplicationDateTime : TDateTime;
    function GetApplicationVersionFull:string;
    function GetApplicationVersionShort:string;
  end;



implementation
uses Forms, Windows, SysUtils;


{ TAApplicationAttributes }


constructor TAApplicationAttributes.Create;
begin
  AttribsList:=TStringList.Create;
  Initailize;
  LoadApplicationDateTime;
  LoadApplicationAttributes;
end;

destructor TAApplicationAttributes.Destroy;
begin
  AttribsList.Free;
  inherited;
end;

procedure TAApplicationAttributes.Initailize;
begin
  ApplicationFilePath:=Application.ExeName;
  ApplicationVersion:='';
  ApplicationDateTime:=0;
end;

procedure TAApplicationAttributes.LoadApplicationDateTime;
begin
  FileAge(ApplicationFilePath, ApplicationDateTime);
end;

procedure TAApplicationAttributes.LoadApplicationAttributes;
const
  InfoNum = 10;
  InfoStr: array[1..InfoNum] of string = ('CompanyName', 'FileDescription', 'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks', 'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');
type
  TLangChrSet = array[0..1] of word;
  PLangChrset = ^TLangChrSet;
var
  LangChrSet: PLangChrSet;
  LangCodePage : string;
var
  n, Len, i: LongWord;
  Buf: PChar;
  Value: PChar;
begin
  n := GetFileVersionInfoSize(PChar(ApplicationFilePath), n);
  if n > 0 then
  begin
    Buf := AllocMem(n);
    GetFileVersionInfo(PChar(ApplicationFilePath), 0, n, Buf);
    VerQueryValue(Buf, PChar('VarFileInfo\Translation'),pointer(LangChrset), Len);
    LangCodePage:=Format('%.4x%.4x',[LangChrSet^[0], LangChrSet^[1]]);
    for i := 1 to InfoNum do
      if VerQueryValue(Buf, PChar('StringFileInfo\' + LangCodePage + '\' + InfoStr[i]), Pointer(Value), Len)
        then AttribsList.Add(InfoStr[i] + '=' + Value);
    FreeMem(Buf, n);
  end;
  ApplicationVersion:=AttribsList.Values['FileVersion'];
end;

function TAApplicationAttributes.GetApplicationVersionFull: string;
var
  VersionElements : TStringList;
begin
  Result:='';
  VersionElements:=TStringList.Create;
  VersionElements.Delimiter:='.';
  VersionElements.DelimitedText:=ApplicationVersion;
  if (VersionElements.Count>=4) then Result:=VersionElements[0]+'.'+VersionElements[1]+'.'+VersionElements[2]+'.'+VersionElements[3];
end;

function TAApplicationAttributes.GetApplicationVersionShort: string;
var
  VersionElements : TStringList;
begin
  Result:='';
  VersionElements:=TStringList.Create;
  VersionElements.Delimiter:='.';
  VersionElements.DelimitedText:=ApplicationVersion;
  if (VersionElements.Count>=2) then Result:=VersionElements[0]+'.'+VersionElements[1];
end;

end.
