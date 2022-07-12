unit RestAPI;

interface
uses  System.Classes,IdTCPConnection, IdTCPClient, IdIOHandler,System.DateUtils,System.ZLib,IdURI,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,System.NetEncoding,SysUtils,IdGlobal, IdHTTP;

type
  TLogLevel = (lvOff,lvInfo,lvAll);
  TConnectMethod = (cmGet,cmPost,cmPut,cmDelete);
  TOnRestAPIEvent = procedure(LogLevel:TLogLevel;EventMessage: string) of object;

  TRestAPI = class
  protected
    FOnRestAPIEvent:TOnRestAPIEvent;
    FIdTCPClient: TIdTCPClient;
    FIdTCPSSLIOHandlerSocketOpenSSL:TIdSSLIOHandlerSocketOpenSSL;
    FIdHTTPClient: TIdHTTP;
    FIdSSLIOHandlerSocketOpenSSL:TIdSSLIOHandlerSocketOpenSSL;
    procedure DoOnRestAPIEvent(LogLevel:TLogLevel;EventMessage:String);
  public
    property OnRestAPIEvent:TOnRestAPIEvent write FOnRestAPIEvent;
    constructor Create;
    function Connect(URL:String;var HttpHeaders,HttpResult:String;ConnectMethod:TConnectMethod = cmGet;CustomHeaders:TStringList = Nil;PostParams:TStringList = Nil):Boolean;
    function TCPConnect(URL:String;var HttpHeaders,HttpResult:String;ConnectMethod:TConnectMethod = cmGet;CustomHeaders:TStringList = Nil;PostParams:TStringList = Nil):Boolean;
    destructor Destroy; override;
  end;

implementation

constructor TRestAPI.Create;
begin
  inherited Create;
  FIdHTTPClient := TIdHTTP.Create(Nil);
  FIdHTTPClient.Request.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36';
  FIdHTTPClient.Request.Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3';
  FIdHTTPClient.Request.AcceptLanguage := 'en-US,en;q=0.9,fa;q=0.8';
  FIdHTTPClient.Request.ContentType := 'application/json';

  FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
  FIdHTTPClient.IOHandler := FIdSSLIOHandlerSocketOpenSSL;


  FIdTCPClient := TIdTCPClient.Create(Nil);
  FIdTCPClient.ReadTimeout := 10000;
  FIdTCPSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
  FIdTCPSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
  FIdTCPClient.IOHandler := FIdTCPSSLIOHandlerSocketOpenSSL;
end;

destructor TRestAPI.Destroy;
begin
  inherited Destroy;
  FIdHTTPClient.Destroy;
  FIdSSLIOHandlerSocketOpenSSL.Destroy;
  FIdTCPClient.Destroy;
  FIdTCPSSLIOHandlerSocketOpenSSL.Destroy;
end;

function TRestAPI.TCPConnect(URL:String;var HttpHeaders,HttpResult:String;ConnectMethod:TConnectMethod = cmGet;CustomHeaders:TStringList = Nil;PostParams:TStringList = Nil):Boolean;
var URI: TIdURI;
    RecvData,HeaderData:String;
    HttpResultStream:TMemoryStream;
    Chunked:Boolean;
    AByteCount:Integer;
    VBuffer:TIdBytes;
    F:TextFile;
begin
  try
    try
      Result := False;
      HttpHeaders := '';
      HttpResult := '';

      URI := TIdURI.Create(URL);
      if URI.Port = '' then if URI.Protocol.ToUpper = 'HTTPS' Then URI.Port := '443' else URI.Port := '80';

      FIdTCPClient.Disconnect;
      FIdTCPClient.Host := URI.Host;
      FIdTCPClient.Port := StrToInt(URI.Port);
      FIdTCPClient.Connect;
      With FIdTCPClient Do
      begin
        case ConnectMethod of
          cmGet: IOHandler.WriteLn('GET '+URI.Path+URI.Document+'?'+URI.Params+' HTTP/1.1');
          cmPost: IOHandler.WriteLn('POST '+URI.Path+URI.Document+'?'+URI.Params+' HTTP/1.1');
          cmPut: IOHandler.WriteLn('PUT '+URI.Path+URI.Document+'?'+URI.Params+' HTTP/1.1');
          cmDelete: IOHandler.WriteLn('DELETE '+URI.Path+URI.Document+'?'+URI.Params+' HTTP/1.1');
        end;

        IOHandler.WriteLn('Host: '+URI.Host);
        IOHandler.WriteLn('User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36');
        IOHandler.WriteLn('Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3');
        IOHandler.WriteLn('Accept-Language: en-US,en;q=0.9,fa;q=0.8');
        IOHandler.WriteLn(StringReplace(CustomHeaders.Text,'=',':',[rfReplaceAll]));
        IOHandler.WriteLn('');
        if PostParams <> Nil then   IOHandler.WriteLn(PostParams.Text);

        HttpHeaders := '';
        repeat
          RecvData := IOHandler.ReadLn(enUTF8);
          if RecvData <> '' then
            HttpHeaders := HttpHeaders + RecvData + #13#10;
        until RecvData = '';

//        'HTTP/1.1 200 OK'

        Chunked := False;
        if Pos('CHUNKED',HttpHeaders.ToUpper) > 0 then Chunked := True;


        HttpResult := '';
        AByteCount := 0;
        repeat
          //RecvData := RecvData + IOHandler.ReadChar;
          //IOHandler.ReadBytes();
          //RecvData := IOHandler.ReadLn(enUTF8);
          IOHandler.ReadBytes(VBuffer,IOHandler.InputBuffer.Size);
          RecvData := BytesToString(VBuffer,enUTF8);
          VBuffer := Nil;

          if Chunked And (Length(RecvData) > 0) then
          begin
            Try
              AByteCount := StrToInt64('$'+RecvData);
              IOHandler.ReadBytes(VBuffer,AByteCount);
              RecvData := BytesToString(VBuffer,enUTF8);
              SetLength(VBuffer,0);
              VBuffer := Nil;
              IOHandler.ReadLn(enUTF8);
            Except
              AByteCount := 0;
              Chunked := False;
            End;
          end;

          if RecvData <> '' then
            HttpResult := HttpResult + RecvData ; //+ #13#10;

        until not IOHandler.CheckForDataOnSource(5); //(not Chunked And (Length(RecvData) = 0)) OR (Chunked And (AByteCount = 0));

        FIdTCPClient.Disconnect;

        case ConnectMethod of
          cmGet: DoOnRestAPIEvent(lvAll,'Get Connect Request Successfully: '+URL);
          cmPost: DoOnRestAPIEvent(lvAll,'Post Connect Request Successfully: '+URL);
          cmPut: DoOnRestAPIEvent(lvAll,'Put Connect Request Successfully: '+URL);
          cmDelete: DoOnRestAPIEvent(lvAll,'Delete Connect Request Successfully: '+URL);
        end;
        Result := True;
      end;
    finally
      FIdTCPClient.Disconnect;
      URI.Free;
    end;
  except
    on E : Exception do
    begin
      DoOnRestAPIEvent(lvAll,'Exception: '+E.Message);
    end;
  end;
end;

procedure TRestAPI.DoOnRestAPIEvent(LogLevel:TLogLevel;EventMessage:String);
begin
  if Assigned(FOnRestAPIEvent) then FOnRestAPIEvent(LogLevel,EventMessage);
end;

function TRestAPI.Connect(URL:String;var HttpHeaders,HttpResult:String;ConnectMethod:TConnectMethod = cmGet;CustomHeaders:TStringList = Nil;PostParams:TStringList = Nil):Boolean;
var ASource:TStringStream;
begin
  Result := False;
  HttpResult := '';
  try
    DoOnRestAPIEvent(lvAll,'Connect Request: '+URL);

    FIdHTTPClient.Request.CustomHeaders.Clear;
    if CustomHeaders <> Nil then
      FIdHTTPClient.Request.CustomHeaders.AddStrings(CustomHeaders);

    if ConnectMethod = cmDelete then
    begin
      HttpResult := FIdHTTPClient.Delete(URL);
      HttpHeaders := FIdHTTPClient.Response.CustomHeaders.Text;
      Result := True;
      DoOnRestAPIEvent(lvAll,'Delete Connect Request Successfully: '+URL);
    end;


    if ConnectMethod = cmPut then
    begin
      try
        ASource := TStringStream.Create;
        HttpResult := FIdHTTPClient.Put(URL,ASource);
        HttpHeaders := FIdHTTPClient.Response.CustomHeaders.Text;
      finally
        ASource.Free;
      end;
      Result := True;
      DoOnRestAPIEvent(lvAll,'Put Connect Request Successfully: '+URL);
    end;

    if ConnectMethod = cmGet then
    begin
      HttpResult := FIdHTTPClient.Get(URL);
      HttpHeaders := FIdHTTPClient.Response.CustomHeaders.Text;
      Result := True;
      DoOnRestAPIEvent(lvAll,'Get Connect Request Successfully: '+URL);
    end;

    if ConnectMethod = cmPost then
    begin
      if PostParams = Nil then
      begin
        try
          PostParams := TStringList.Create;
          HttpResult := FIdHTTPClient.Post(URL,PostParams,enUTF8);
          HttpHeaders := FIdHTTPClient.Response.CustomHeaders.Text;
        finally
          PostParams.Free;
        end;
      end
      else
      begin
        HttpResult := FIdHTTPClient.Post(URL,PostParams,enUTF8);
      end;
      Result := True;
      DoOnRestAPIEvent(lvAll,'Post Connect Request Successfully: '+URL);
    end;
  except
    on E : Exception do
    begin
      DoOnRestAPIEvent(lvAll,'Exception: '+E.Message);
    end;
  end;
end;

end.
