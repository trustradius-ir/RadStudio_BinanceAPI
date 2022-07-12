unit WebSocket;

interface

{
Frame format:
​​
      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-------+-+-------------+-------------------------------+
     |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
     |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
     |N|V|V|V|       |S|             |   (if payload len==126/127)   |
     | |1|2|3|       |K|             |                               |
     +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
     |     Extended payload length continued, if payload len == 127  |
     + - - - - - - - - - - - - - - - +-------------------------------+
     |                               |Masking-key, if MASK set to 1  |
     +-------------------------------+-------------------------------+
     | Masking-key (continued)       |          Payload Data         |
     +-------------------------------- - - - - - - - - - - - - - - - +
     :                     Payload Data continued ...                :
     + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
     |                     Payload Data continued ...                |
     +---------------------------------------------------------------+
}

uses  System.Classes,IdTCPConnection, IdTCPClient, IdIOHandler,System.DateUtils,System.ZLib, IdURI,Vcl.ExtCtrls,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,System.NetEncoding,SysUtils,IdGlobal,Math;


type
  TWSDataCode      = (wdcContinuation, wdcText, wdcBinary, wdcReserver3, wdcReserver4, wdcReserver5, wdcReserver6 , wdcReserver7, wdcClose, wdcPing, wdcPong);
  TCompressionType = (ctNone,ctDeflate,ctGZip);
  TOnWebSocketEvent = procedure(FSecWebSocketKey:String;EventMessage: string) of object;
  TOnWebSocketData = procedure(FSecWebSocketKey:String;ReceivedData: string) of object;
  TOnWebSocketTimer = procedure(FSecWebSocketKey:String) of object;

  TSendDataRecord = record
    DataToSend:TIdBytes;
    CompressionType:TCompressionType;
    UseMask:Boolean;
  end;

  TSendDataRecords =  array of TSendDataRecord;

  TWebSocket = class(TThread)
  protected
    FOnWebSocketEvent: TOnWebSocketEvent;
    FOnWebSocketData: TOnWebSocketData;
    FOnWebSocketTimer:TOnWebSocketTimer;
    FIdTCPClient: TIdTCPClient;
    FIdSSLIOHandlerSocketOpenSSL:TIdSSLIOHandlerSocketOpenSSL;
    FSendDataQueue:TSendDataRecords;
    FSocketHost:String;
    FSocketPort:Word;
    FSSLConnect:Boolean;
    FSecWebSocketKey:String;
    FHeaderPass:Boolean;
    FRequestURL:String;
    FConnect:Boolean;
    FEventMessage:String;
    FDataMessage:String;
    FConnectionEstablished:Boolean;
    FClosedConnectionRequested:Boolean;
    FAutoPingRequest:Boolean;
    FLastAutoPingRequest:TDateTime;
    FPostMethod:Boolean;
    FCustomHeaders:TStringList;
    FTimerInterval:Integer;
    procedure Execute; override;
    procedure DoOnWebSocketEvent;
    procedure DoOnWebSocketData;
    procedure DoOnWebSocketTimer;
  public
    property TimerInterval:Integer read FTimerInterval write FTimerInterval;
    property SocketHost:String read FSocketHost;
    property CustomHeaders:TstringList read FCustomHeaders write FCustomHeaders;
    property SecWebSocketKey:string read FSecWebSocketKey  write FSecWebSocketKey;
    property AutoPingRequest:Boolean read FAutoPingRequest write FAutoPingRequest;
    property PostMethod:Boolean read FPostMethod write FPostMethod;
    property OnWebSocketEvent:TOnWebSocketEvent write FOnWebSocketEvent;
    property OnWebSocketData:TOnWebSocketData write FOnWebSocketData;
    property OnWebSocketTimer:TOnWebSocketTimer write FOnWebSocketTimer;
    constructor Create(URL:String;SSLConnect:Boolean = True);overload;
    constructor Create(SocketHost:String;SocketPort:Word = 443;SSLConnect:Boolean = True);overload;
    destructor Destroy; override;
    procedure Connect(RequestURL:String = '');
    procedure Disconnect;
    procedure Send(DataToSend:String;UseMask:Boolean = True;CompressionType:TCompressionType= ctNone);
  end;

  function RandomString(PLen: Integer): string;
  function RandomNumber(PLen: Integer): string;
  function WordToString(InValue:Word): AnsiString;
  function CardinalToString(InValue:Cardinal): AnsiString;
  function CompressBytesAsBytes(BytesToCompress:TIdBytes;CompressType:TCompressionType):TIdBytes;
  function CompressStringAsBytes(TextToCompress:String;CompressType:TCompressionType):TIdBytes;
  function DecompressBytesAsBytes(VBuffer:TIdBytes;DecompressType:TCompressionType):TIdBytes;
  function DecompressBytesAsString(VBuffer:TIdBytes;DecompressType:TCompressionType):String;

implementation

function RandomString(PLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str    := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = PLen)
end;

function RandomNumber(PLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str    := '1234567890';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = PLen)
end;

function WordToString(InValue:Word): AnsiString;
begin
  Result:=#0#0;
  Result[1] := AnsiChar(InValue Div 256);
  Result[2] := AnsiChar(InValue Mod 256);
end;


function CardinalToString(InValue:Cardinal): AnsiString;
begin
  Result:=#0#0#0#0;
  Result[1] := AnsiChar(InValue Div 16777216);
  InValue := InValue Mod 16777216;

  Result[2] := AnsiChar(InValue Div 65536);
  InValue := InValue Mod 65536;

  Result[3] := AnsiChar(InValue Div 256);

  Result[4] := AnsiChar(InValue Mod 256);
end;



function CompressBytesAsBytes(BytesToCompress:TIdBytes;CompressType:TCompressionType):TIdBytes;
var CompressionStream: TZCompressionStream;
    CompressedBytesStream:TBytesStream;
begin
  if Length(BytesToCompress) = 0 then Exit;

  if CompressType = ctNone  then
  begin
    Result := BytesToCompress;
    Exit;
  end;

  if CompressType = ctDeflate  then
  begin
    Try
      CompressedBytesStream := TBytesStream.Create();
      Try
        CompressionStream := TZCompressionStream.Create(clDefault,CompressedBytesStream);
        CompressionStream.Write(BytesToCompress[0],Length(BytesToCompress));
      Finally
        CompressionStream.Free;
      End;
      SetLength(Result,CompressedBytesStream.Size);
      CompressedBytesStream.Position := 0;
      CompressedBytesStream.Read(Result[0],CompressedBytesStream.Size);
    Finally
      CompressedBytesStream.Free;
    End;
  end;

  if CompressType = ctGZip  then
  begin
    Try
      CompressedBytesStream := TBytesStream.Create();
      Try
        CompressionStream := TZCompressionStream.Create(CompressedBytesStream, zcMax , 15 + 16);
        CompressionStream.Write(BytesToCompress[0],Length(BytesToCompress));
      Finally
        CompressionStream.Free;
      End;
      SetLength(Result,CompressedBytesStream.Size);
      CompressedBytesStream.Position := 0;
      CompressedBytesStream.Read(Result[0],CompressedBytesStream.Size);
    Finally
      CompressedBytesStream.Free;
    End;
  end;
end;

function CompressStringAsBytes(TextToCompress:String;CompressType:TCompressionType):TIdBytes;
var VTempBuffer:TIdBytes;
    CompressionStream: TZCompressionStream;
    CompressedBytesStream:TBytesStream;
begin
  if Length(TextToCompress) = 0 then Exit;

  if CompressType = ctNone  then
  begin
    Result := ToBytes(TextToCompress,enUTF8);
    Exit;
  end;

  VTempBuffer := ToBytes(TextToCompress,enUTF8);
  Result := CompressBytesAsBytes(VTempBuffer,CompressType);
end;

function DecompressBytesAsBytes(VBuffer:TIdBytes;DecompressType:TCompressionType):TIdBytes;
var
  CompressionType:TCompressionType;
  CompressedBytesStream:TBytesStream;
  DecompressionStream: TZDecompressionStream;
  DecompressionString:TStringStream;
begin
  if DecompressType = ctNone  then
  begin
    Result := VBuffer;
    Exit;
  end;

  if DecompressType = ctDeflate then
  begin
    Try
      CompressedBytesStream:= TBytesStream.Create;
      CompressedBytesStream.WriteBuffer(VBuffer[0],Length(VBuffer));
      CompressedBytesStream.Position := 0 ;

      DecompressionStream := TDecompressionStream.Create(CompressedBytesStream);
      DecompressionString := TStringStream.Create('', TEncoding.UTF8);
      DecompressionString.LoadFromStream(DecompressionStream);
      SetLength(Result,DecompressionString.Size);
      DecompressionString.Read(Result[0],DecompressionString.Size);
    Finally
      DecompressionStream.Free;
      DecompressionString.Free;
      CompressedBytesStream.Free;
    End;
  end;

  if DecompressType = ctGZip then
  begin
    Try
      CompressedBytesStream:= TBytesStream.Create;
      CompressedBytesStream.WriteBuffer(VBuffer[0],Length(VBuffer));
      CompressedBytesStream.Position := 0 ;

      DecompressionStream := TDecompressionStream.Create(CompressedBytesStream,15 + 16);
      DecompressionString := TStringStream.Create('', TEncoding.UTF8);
      DecompressionString.LoadFromStream(DecompressionStream);
      SetLength(Result,DecompressionString.Size);
      DecompressionString.Read(Result[0],DecompressionString.Size);
    Finally
      DecompressionStream.Free;
      DecompressionString.Free;
      CompressedBytesStream.Free;
    End;
  end;

end;

function DecompressBytesAsString(VBuffer:TIdBytes;DecompressType:TCompressionType):String;
var
  CompressionType:TCompressionType;
  CompressedBytesStream:TBytesStream;
  DecompressionStream: TZDecompressionStream;
  DecompressionString:TStringStream;
begin
  Result := '';

  if DecompressType = ctNone  then
  begin
    Result := BytesToString(VBuffer,enUTF8);
  end
  else
    Result := BytesToString(DecompressBytesAsBytes(VBuffer,DecompressType),enUTF8);

  SetLength(VBuffer,0);
  VBuffer := Nil;
end;



procedure TWebSocket.DoOnWebSocketEvent;
begin
  if Assigned(FOnWebSocketEvent) then FOnWebSocketEvent(FSecWebSocketKey,FEventMessage);
end;

procedure TWebSocket.DoOnWebSocketData;
begin
  if Assigned(FOnWebSocketData) then FOnWebSocketData(FSecWebSocketKey,FDataMessage);
end;

procedure TWebSocket.DoOnWebSocketTimer;
begin
  if Assigned(FOnWebSocketTimer) then FOnWebSocketTimer(FSecWebSocketKey);
end;


procedure TWebSocket.Send(DataToSend:String;UseMask:Boolean = True;CompressionType:TCompressionType= ctNone);
begin
  SetLength(FSendDataQueue,Length(FSendDataQueue) + 1);
  FSendDataQueue[Length(FSendDataQueue)-1].UseMask := UseMask;
  FSendDataQueue[Length(FSendDataQueue)-1].CompressionType := CompressionType;
  FSendDataQueue[Length(FSendDataQueue)-1].DataToSend := ToBytes(DataToSend,enUTF8);
end;


constructor TWebSocket.Create(URL:String;SSLConnect:Boolean = True);
var URI: TIdURI;
begin
  try
    URI := TIdURI.Create(URL);
    if URI.Port = '' then if SSLConnect Then URI.Port := '443' else URI.Port := '80';
    Create(URI.Host,StrToInt(URI.Port),SSLConnect);
    FRequestURL := URI.GetPathAndParams;
  finally
    URI.Free;
  end;
end;


constructor TWebSocket.Create(SocketHost:String;SocketPort:Word = 443;SSLConnect:Boolean = True);
begin
  inherited Create(False);
  FSocketHost := SocketHost;
  FSocketPort := SocketPort;
  FSSLConnect := SSLConnect;
  FHeaderPass := False;
  Randomize;
  FSecWebSocketKey := TNetEncoding.Base64.Encode(IntToStr(Random(99999998)+10000000)+IntToStr(Random(99999998)+10000000));
  FRequestURL:= '';
  FConnect := False;
  FConnectionEstablished := False;
  FClosedConnectionRequested := False;
  FAutoPingRequest := False;
  FLastAutoPingRequest := Now;
  FCustomHeaders := TStringList.Create;
  FTimerInterval := 0;

  FIdTCPClient := TIdTCPClient.Create(Nil);
  FIdTCPClient.ReadTimeout := 10000;
  if FSSLConnect then
  begin
    FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    FIdSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
    FIdTCPClient.IOHandler := FIdSSLIOHandlerSocketOpenSSL;
  end;
  FIdTCPClient.Host := FSocketHost;
  FIdTCPClient.Port := FSocketPort;
end;


procedure TWebSocket.Connect(RequestURL:String = '');
begin
  if Length(RequestURL) > 0 then FRequestURL := RequestURL;
  FClosedConnectionRequested := False;
  FConnect := True;
end;

procedure TWebSocket.Disconnect;
begin
  FClosedConnectionRequested := True;
  FConnect := False;
end;


destructor TWebSocket.Destroy;
begin
  inherited Destroy;
  FIdTCPClient.Destroy;
  FCustomHeaders.Destroy;
end;



procedure TWebSocket.Execute;
var ResponseLine:String;
    FrameB1,FrameB2,iByte:Byte;
    BitFin,BitRSV1,BitRSV2,BitRSV3,BitOPCode,BitMask,BitPayLoad:Byte;
    MaskingKey:Array[0..3] of Byte;
    ExtPayLoad:UInt64;
    ExtPayLoad16:UInt16;
    SDataCode:TWSDataCode;
    SBuffer,VBuffer,DummyHead:TIdBytes;
    Buffer:TBytes;
    DataStreamText:String;
    I,LengthDataToSend:Integer;
    MaskBit:Byte;
    UseMask:Boolean;
    CompressionType:TCompressionType;
    LastTimerInterval:TDateTime;
begin
  LastTimerInterval := Now;
  while not Terminated do
  begin
    Sleep(10);
    Try
      if FTimerInterval > 0 then
      begin
        if MilliSecondsBetween(Now,LastTimerInterval) > FTimerInterval then
        begin
          LastTimerInterval := Now;
          DoOnWebSocketTimer;
        end;
      end;

      //Connect Requested And Process it
      if FConnect And not FIdTCPClient.Connected And FConnectionEstablished Then
      begin
        FEventMessage := 'Socket UnPermantly Disconnected';
        Synchronize(DoOnWebSocketEvent);
      end;

      //Connect Requested And Process it
      if FConnect And not FIdTCPClient.Connected Then
      begin
        With FIdTCPClient Do
        begin
          FHeaderPass := False;
          FConnectionEstablished := False;

          Connect;

          FEventMessage := 'Socket Connected';
          Synchronize(DoOnWebSocketEvent);

          {if FPostMethod then
            IOHandler.WriteLn('POST wss://'+FSocketHost+FRequestURL+' HTTP/1.1')
          else
            IOHandler.WriteLn('GET wss://'+FSocketHost+FRequestURL+' HTTP/1.1');  }

          if FPostMethod then
            IOHandler.WriteLn('POST '+FRequestURL+' HTTP/1.1')
          else
            IOHandler.WriteLn('GET '+FRequestURL+' HTTP/1.1');

          IOHandler.WriteLn('Host: '+FSocketHost);
          IOHandler.WriteLn('Upgrade: websocket');
          IOHandler.WriteLn('Connection: upgrade');
          IOHandler.WriteLn('Sec-WebSocket-Key: '+FSecWebSocketKey);
          IOHandler.WriteLn('Sec-Websocket-Version: 13');
          if FCustomHeaders.Count >0  then
            IOHandler.Write(FCustomHeaders);

          IOHandler.WriteLn('');
        end;
      end;

      //Disconnect Requested And Process it
      if not FConnect And FIdTCPClient.IOHandler.Connected And (FIdTCPClient.IOHandler.InputBuffer.Size = 0) Then
      begin
        FEventMessage := 'Socket Disconnected';
        Synchronize(DoOnWebSocketEvent);

        //FIdTCPClient.IOHandler.InputBuffer.Clear;
        FIdTCPClient.Disconnect;

        Continue;
      end;


      if not FIdTCPClient.Connected then Continue;

      //Disconnect Requested And Process it
      if FClosedConnectionRequested And FConnectionEstablished then
      begin
        FClosedConnectionRequested := False;

        FEventMessage := 'Closed Connection Frame Send';
        Synchronize(DoOnWebSocketEvent);

        SetLength(VBuffer,6);
        VBuffer[0] := $88; //FrameB1
        VBuffer[1] := $80; //FrameBB
        VBuffer[2] := $0; //Mask 0
        VBuffer[3] := $0; //Mask 1
        VBuffer[4] := $0; //Mask 2
        VBuffer[5] := $0; //Mask 3

        FIdTCPClient.IOHandler.Write(VBuffer,Length(VBuffer));

        SetLength(VBuffer,0);
        VBuffer := Nil;

        Continue;
      end;

      //Ping Requested
      if FAutoPingRequest And FConnectionEstablished And (SecondsBetween(Now,FLastAutoPingRequest) > 30) then
      begin
        FLastAutoPingRequest := Now;

        FEventMessage := 'Ping Frame Send';
        Synchronize(DoOnWebSocketEvent);

        SetLength(VBuffer,6);
        VBuffer[0] := $89; //FrameB1
        VBuffer[1] := $80; //FrameBB
        VBuffer[2] := $0; //Mask 0
        VBuffer[3] := $0; //Mask 1
        VBuffer[4] := $0; //Mask 2
        VBuffer[5] := $0; //Mask 3

        FIdTCPClient.IOHandler.Write(VBuffer,Length(VBuffer));
        FIdTCPClient.IOHandler.WriteBufferFlush;

        SetLength(VBuffer,0);
        VBuffer := Nil;

        Continue;
      end;

      //Send Data
      if FConnectionEstablished And (Length(FSendDataQueue) > 0) then
      begin
        SBuffer := FSendDataQueue[0].DataToSend;
        UseMask := FSendDataQueue[0].UseMask;
        CompressionType := FSendDataQueue[0].CompressionType;

        VBuffer := CompressBytesAsBytes(SBuffer,CompressionType);

        LengthDataToSend := Length(VBuffer);

        if UseMask Then MaskBit := 128 Else MaskBit := 0;

        FIdTCPClient.IOHandler.Write(Ord(AnsiChar(#$81)));
        if LengthDataToSend < 125 then
          FIdTCPClient.IOHandler.Write(Ord(AnsiChar(MaskBit+LengthDataToSend)));

        if (LengthDataToSend > 125) And (LengthDataToSend <= 65535) then
        begin
          FIdTCPClient.IOHandler.Write(Ord(AnsiChar(MaskBit+126)));
          FIdTCPClient.IOHandler.Write(WordToString(LengthDataToSend));
        end;

        if (LengthDataToSend > 65535)  then
        begin
          FIdTCPClient.IOHandler.Write(Ord(AnsiChar(MaskBit+127)));
          FIdTCPClient.IOHandler.Write(CardinalToString(LengthDataToSend));
        end;

        if UseMask then
        begin
          Randomize;
          MaskingKey[0] := Random(254);
          MaskingKey[1] := Random(254);
          MaskingKey[2] := Random(254);
          MaskingKey[3] := Random(254);

          for I := 0 to 3 do
            FIdTCPClient.IOHandler.Write(MaskingKey[i]);

          for I := 0 to LengthDataToSend-1 do
            VBuffer[I] := VBuffer[I] XOR MaskingKey[I Mod 4];
        end;

        FIdTCPClient.IOHandler.Write(VBuffer);
        FIdTCPClient.IOHandler.WriteBufferFlush;

        SetLength(SBuffer,0);
        SBuffer := Nil;

        SetLength(VBuffer,0);
        VBuffer := Nil;

        SetLength(FSendDataQueue[0].DataToSend,0);
        FSendDataQueue[0].DataToSend := Nil;

        Delete(FSendDataQueue,0,1);

        Continue;
      end;

      if FIdTCPClient.IOHandler.InputBuffer.Size = 0 then Continue;

      //Skip First Connect Headers
      if not FHeaderPass then
      begin
        ResponseLine := FIdTCPClient.IOHandler.ReadLn;

        if (ResponseLine <> 'HTTP/1.1 101 Switching Protocols') And (Copy(ResponseLine,1,4) = 'HTTP') Then
        begin
          FConnectionEstablished := False;
          FEventMessage := ResponseLine;
          Synchronize(DoOnWebSocketEvent);
          FIdTCPClient.Disconnect;
        end;

        if ResponseLine = 'HTTP/1.1 101 Switching Protocols' Then
        begin
          FConnectionEstablished := True;

          FEventMessage := 'HTTP/1.1 101 Switching Protocols';
          Synchronize(DoOnWebSocketEvent);

          FEventMessage := 'WebSocket Protocol Detected';
          Synchronize(DoOnWebSocketEvent);
        end;


        if Length(ResponseLine) = 0 then  FHeaderPass := True;

        Continue;
      end;

      if not FConnectionEstablished then Continue;

      FrameB1 := FIdTCPClient.IOHandler.ReadByte;
      BitFin := FrameB1 And 128;//10000000;
      BitRSV1 := FrameB1 And 64; //01000000;
      BitRSV2 := FrameB1 And 32; //00100000;
      BitRSV3 := FrameB1 And 16; //00010000;
      BitOPCode := FrameB1 And 15; //00001111;
      SDataCode := TWSDataCode(BitOPCode);

      FrameB2 := FIdTCPClient.IOHandler.ReadByte;
      BitMask := FrameB2 And 128 ; //10000000;
      BitPayLoad := FrameB2 And 127; //1111111;

      if BitMask = 1 then
      begin
        MaskingKey[0] := FIdTCPClient.IOHandler.ReadByte;
        MaskingKey[1] := FIdTCPClient.IOHandler.ReadByte;
        MaskingKey[2] := FIdTCPClient.IOHandler.ReadByte;
        MaskingKey[3] := FIdTCPClient.IOHandler.ReadByte;
      end;

      ExtPayLoad := 0;
      if BitPayLoad < 125 then ExtPayLoad := BitPayLoad;
      if BitPayLoad = 126 then
      begin
        ExtPayLoad16 := FIdTCPClient.IOHandler.ReadUInt16;
        ExtPayLoad := ExtPayLoad16;
      end;
      if BitPayLoad = 127 then ExtPayLoad := FIdTCPClient.IOHandler.ReadUInt64;

      if ExtPayLoad > 0 then
      begin
        FIdTCPClient.IOHandler.ReadBytes(VBuffer,ExtPayLoad);
      end;

      if BitMask = 1 then
      begin
        for I := 0 to Length(VBuffer)-1 do
          VBuffer[I] := VBuffer[I] XOR MaskingKey[I Mod 4];
      end;

      if SDataCode = wdcText then
      begin
        FDataMessage := AnsiToUtf8(BytesToString(VBuffer));
        Synchronize(DoOnWebSocketData);

        SetLength(VBuffer,0);
        VBuffer := Nil;

        Continue;
      end;

      if SDataCode =  wdcBinary then
      begin
        CompressionType := ctNone;
        if (VBuffer[0] = $78) And ((VBuffer[1] = $01) OR (VBuffer[1] = $5E) OR (VBuffer[1] = $9C) OR (VBuffer[1] = $DA)) then CompressionType := ctDeflate;
        if (VBuffer[0] = $1F) And (VBuffer[1] = $8b) then CompressionType := ctGZip;

        FDataMessage := '';
        if CompressionType = ctNone then
          FDataMessage := TNetEncoding.Base64.EncodeBytesToString(VBuffer);

        FDataMessage := DecompressBytesAsString(VBuffer,CompressionType);

        SetLength(VBuffer,0);
        VBuffer := Nil;

        Synchronize(DoOnWebSocketData);
        Continue;
      end;

      if SDataCode = wdcClose then
      begin
        SetLength(VBuffer,0);
        VBuffer := Nil;

        FIdTCPClient.Disconnect;

        FConnectionEstablished := False;
        FHeaderPass := False;
        FConnect := False;

        FEventMessage := 'Receive Closed Connection Frame';
        Synchronize(DoOnWebSocketEvent);

        Continue;
      end;


      if SDataCode =  wdcPing then
      begin
        SetLength(VBuffer,0);
        VBuffer := Nil;

        FEventMessage := 'Ping Frame Received and Pong Frame Responses';
        Synchronize(DoOnWebSocketEvent);

        SetLength(VBuffer,6);
        VBuffer[0] := $8A; //FrameB1
        VBuffer[1] := $80; //FrameBB
        VBuffer[2] := $0; //Mask 0
        VBuffer[3] := $0; //Mask 1
        VBuffer[4] := $0; //Mask 2
        VBuffer[5] := $0; //Mask 3

        FIdTCPClient.IOHandler.Write(VBuffer,Length(VBuffer));
        FIdTCPClient.IOHandler.WriteBufferFlush;

        SetLength(VBuffer,0);
        VBuffer := Nil;

        Continue;
      end;

      if SDataCode =  wdcPong then
      begin
        FEventMessage := 'Pong Frame Received';
        Synchronize(DoOnWebSocketEvent);

        SetLength(VBuffer,0);
        VBuffer := Nil;

        Continue;
      end;

      if not (SDataCode in [wdcText,wdcBinary,wdcPing]) then
      begin
        FEventMessage := 'Unknown Packet Received';
        Synchronize(DoOnWebSocketEvent);
      end;

      if VBuffer <> Nil then
      begin
        SetLength(VBuffer,0);
        VBuffer := Nil;
      end;


    Except
      on E : Exception do
      begin
        FEventMessage := 'Exception: '+E.Message;
        Synchronize(DoOnWebSocketEvent);

        FConnect := False;
      end;
    End;
  end;
end;


end.
