program main;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, fphttpapp, httpdefs, httproute, fpjson, jsonparser,
  Dos,
  Classes, server, notesrv;

var
  rootPath: string;
  portStr: string;
  port: integer;
  code: integer;
  hnd: TNotesAppHandler;
begin
  rootPath := ExtractFilePath(ParamStr(0));
  // port := defaultPort;
  portStr := GetEnv('PORT');
  Val(portStr, port, code);
  // if ParamCount >= 1 then port := StrToIntDef(ParamStr(1), defaultPort);

  hnd := TNotesAppHandler.Create(rootPath);
  try
    Application.Port := port;

    // notes routes
    HTTPRouter.RegisterRoute('/api/:id', @hnd.notesApi);

    // static and redirect to index
    HTTPRouter.RegisterRoute('/*', @hnd.static);
    HTTPRouter.RegisterRoute('/', @hnd.indexDir, true);

    WriteLn('Server started on port ', port);

    Application.OnException := @hnd.OnErr;
    Application.Threaded := true;
    Application.Initialize;
    Application.Run;

  finally
    hnd.Free;
  end;
end.
