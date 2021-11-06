unit FormLocation;

interface

uses Windows, Controls, Forms, Registry, SysUtils;

type
  TStoreType = (stLeft, stTop, stWidth, stHeight, stState);
  TStoreOptions = set of TStoreType;

const
  soAll = [stLeft, stTop, stWidth, stHeight, stState];
  soPosOnly = [stLeft, stTop];
  soSizeOnly = [stWidth, stHeight, stState];

procedure StoreFormLocation(const Reg: TRegistry; const FormKey: string;
                            AForm: TForm; StoreOptions: TStoreOptions);

function RetrieveFormLocation(const Reg: TRegistry; const FormKey: string;
                              AForm: TForm; var Params: TCreateParams): Boolean;

procedure FitFormOnScreen(var Params: TCreateParams);

implementation

procedure StoreFormLocation;
var
  WindowPlacement: TWINDOWPLACEMENT;
begin
  with Reg do
  begin
      if OpenKey(FormKey, True) then
      begin
        WindowPlacement.length := sizeof(WindowPlacement);
        if not GetWindowPlacement(AForm.Handle, @WindowPlacement) then
          raise Exception.Create('GetWindowPlacement() failed for ' + FormKey);
        with WindowPlacement.rcNormalPosition do
        begin
          if stLeft   in StoreOptions then WriteInteger('X', Left);
          if stTop    in StoreOptions then WriteInteger('Y', Top);
          if stWidth  in StoreOptions then WriteInteger('W', Right - Left);
          if stHeight in StoreOptions then WriteInteger('H', Bottom - Top);
          if stState  in StoreOptions then WriteInteger('S', Ord(Aform.WindowState));
        end;
      end;
  end;
end;

function RetrieveFormLocation;
var HasLoc, HasSize: boolean;
begin
  Result := False;
  with Reg do
  begin
      if OpenKey(FormKey, False) then
      begin
        HasLoc := ValueExists('X');
        if HasLoc then
          Params.X := ReadInteger('X');
        if ValueExists('Y') then
          Params.Y := ReadInteger('Y');
        HasSize := ValueExists('W');
        if HasSize then
          Params.Width := ReadInteger('W');
        if ValueExists('H') then
          Params.Height := ReadInteger('H');
        if ValueExists('S') then
          AForm.WindowState := TWindowState(ReadInteger('S'));
        if HasLoc then
        begin
          if HasSize then
            AForm.Position := poDefault
          else
            AForm.Position := poDefaultPosOnly;
        end else if HasSize then
          AForm.Position := poDefaultSizeOnly;
      end;
  end;
end;

procedure FitFormOnScreen(var Params: TCreateParams);
begin
    if Params.X < 0 then params.X := 0;
    if Params.Y < 0 then params.Y := 0;
    if params.Width > screen.Width then
      params.Width := screen.Width;
    if params.Height > screen.Height then
      params.Height := screen.Height;
    if params.Y + params.Height > screen.Height then
      params.Y := screen.Height - params.Height;
    if params.X + params.Width > screen.Width then
      params.X := screen.Width - params.Width;
end;

end.
