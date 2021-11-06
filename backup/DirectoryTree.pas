unit DirectoryTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl;

type
  TDirectoryTree = class(TCustomTreeView)
  private
    FImageListHandle: HImageList;
    FNewFolderName: string;
    procedure InitDrives;
    function GetDirectoryFor(node: TTreeNode): string;
    function GetDirectory: string;
    procedure SetDirectory(value: string);
    function CreateItem(ParentNode: TTreeNode; Path, name: string): TTreeNode;
    procedure Edited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure AllowEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
  protected
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Delete(Node: TTreeNode); override;
    procedure CreateWnd; override;
    //procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CurrentDirectory;
    procedure CreateDirectory;
    procedure DeleteDirectory;
    procedure Refresh;
    property Items;
  published
    property Align;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay default 50;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection default False;
    property HotTrack;
    property Indent;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    //property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    //property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    //property OnGetImageIndex;
    //property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // Introduced
    property Directory: string read GetDirectory write SetDirectory;
    property NewFolderName: string read FNewFolderName write FNewFolderName;
  end;

procedure Register;

implementation

uses ShellAPI;

type
  PDirectoryData = ^TDirectoryData;
  TDirectoryData = record
    name: string;
  end;

const
  DIRQUERY =
   SHGFI_DISPLAYNAME or
   SHGFI_SYSICONINDEX or
   SHGFI_SMALLICON or
   SHGFI_ICON;

constructor TDirectoryTree.Create;
begin
  inherited;
  ReadOnly := True;
  HideSelection := False;
  ChangeDelay := 50;
  OnEdited := Edited;
  OnEditing := AllowEditing;
  FNewFolderName := 'New Folder';
  SortType := stText;
  FImageListHandle := INVALID_HANDLE_VALUE;
end;

Destructor TDirectoryTree.Destroy;
begin
  inherited;
end;


procedure TDirectoryTree.Delete;
var d: PDirectoryData;
begin
  d := Node.Data;
  if d <> nil then
  begin
    Node.Data := nil;
    Dispose(d);
  end;
end;

procedure TDirectoryTree.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if (Items.Count = 0) then InitDrives;
end;

procedure TDirectoryTree.InitDrives;
var
  info: TSHFILEINFO;
  node: TTreeNode;
  h: THANDLE;
  buffer: PChar;
  current: PChar;
  buflen: Cardinal;
  data: PDirectoryData;
begin
   buflen := GetLogicalDriveStrings(0, nil);
   GetMem(buffer, buflen);
   GetLogicalDriveStrings(buflen, buffer);
   current := buffer;
   while current[0] <> #0 do
   begin
     fillchar(info, sizeof(info), 0);
     h := SHGetFileInfo(current, 0, info, sizeof(info), DIRQUERY);
     if FImageListHandle = INVALID_HANDLE_VALUE then
     begin
       TreeView_SetImageList(Handle, h, TVSIL_NORMAL);
       FImageListHandle := h;
     end;
     node := Items.Add(nil, info.szDisplayName);
     node.ImageIndex := info.iIcon;
     node.SelectedIndex := info.iIcon;
     new(data);
     data.name := string(current);
     node.Data := data;
     node.HasChildren := true;
     current := current + strlen(current) + 1;
   end;
   FreeMem(buffer, buflen);
end;

function TDirectoryTree.CreateItem;
var
  info: TSHFILEINFO;
  data: PDirectoryData;
const
  query =
   SHGFI_DISPLAYNAME or
   SHGFI_SYSICONINDEX or
   SHGFI_SMALLICON or
   SHGFI_ICON;
begin
  fillchar(info, sizeof(info), 0);
  SHGetFileInfo(PChar(path + name), 0, info, sizeof(info), query);
  Result := Items.AddChild(parentnode, info.szDisplayName);
  Result.ImageIndex := info.iIcon;
  Result.SelectedIndex := info.iIcon;
  Result.HasChildren := true;
  new(data);
  data.name := Name + '\';
  Result.data := data;
end;

function TDirectoryTree.CanExpand;
var
  path: string;
  sr: TSearchRec;
  dirs: TStringList;
  i: Integer;
begin
  Result := Node.HasChildren;
  // If Set to false -> Checked and no dirs found
  // If true -> Not Checked (count=0) or already done (count>0)
  if (not Result) or (Node.Count > 0) then
      Exit;
  Result := False;
  path := GetDirectoryFor(node);
  if findfirst(path + '*.*', faDirectory, sr) = 0 then
  begin
    Dirs := TStringList.Create;
    Dirs.Sorted := True;
    repeat
     if ((sr.Attr and faDirectory) <> 0) and (sr.name <> '.') and (sr.name <> '..') then
     begin
       Dirs.Add(sr.name);
       Result := true;
     end;
    until findnext(sr) <> 0;
    FindClose(sr);
    for i := 0 to Dirs.Count-1 do
       CreateItem(Node, path, Dirs[i]);
    Dirs.Free;
  end;
  if not Result then
    Node.HasChildren := false;
end;

function TDirectoryTree.GetDirectory;
var sel: TTreeNode;
begin
  sel := Selected;
  if assigned(sel) then
    Result := GetDirectoryFor(sel)
  else
    Result := '';
end;

function TDirectoryTree.GetDirectoryFor;
begin
  Result := '';
  while node <> nil do
  begin
    Result := PDirectoryData(node.Data).name + Result;
    node := node.Parent;
  end;
end;

procedure TDirectoryTree.CurrentDirectory;
begin
  Directory := GetCurrentDir;
end;


procedure TDirectoryTree.SetDirectory;
var Node, NodeParent: TTreeNode;
    i: Integer;
    p: integer;
    drive: string;
begin
  if value = '' then Exit;
  if value[length(value)] <> '\' then value := value + '\';
  p := pos('\', value);
  Node := nil;
  drive := copy(value, 0, p);
  value := copy(value, p+1, length(value)-p);
  i := 0;
  while (i < Items.Count) and (Node = nil) do
  begin
    if CompareText(PDirectoryData(items[i].Data).Name, drive) = 0 then
    begin
      Node := items[i]
    end;
    inc(i);
  end;
  if assigned(Node) then
  begin
    while (value <> '') and CanExpand(Node) do
    begin
      NodeParent := Node;
      p := pos('\', value);
      if p = 0 then p := length(value);
      drive := copy(value, 0, p);
      value := copy(value, p+1, length(value)-p);
      i := 0;
      Node := nil;
      while (i < NodeParent.Count) and (Node = nil) do
      begin
        if CompareText(PDirectoryData(NodeParent[i].Data).Name, drive) = 0 then
        begin
          Node := NodeParent[i];
        end;
        inc(i);
      end;
      if Node = nil then
      begin
        Selected := NodeParent;
        Exit;
      end;
    end;
    Selected := Node;
  end;
end;

procedure TDirectoryTree.CreateDirectory;
var Dir, NewDir: string;
    Node: TTreeNode;
begin
  if ReadOnly then Abort;
  Dir := Directory;
  NewDir := Dir + NewFolderName;
  Selected.Expand(false);
  MkDir(NewDir);
  Node := CreateItem(Selected, Dir, NewFolderName);
  Node.HasChildren := False;
  Selected.Expand(false);
  SetFocus;
  Node.EditText;
end;

procedure TDirectoryTree.DeleteDirectory;
var Dir: string;
    Current: string;
    Node: TTreeNode;
begin
  Node := Selected;
  Node.Expand(false);
  if ReadOnly then Abort;
  if (Node.Parent = nil) then
    raise Exception.Create('Cannot delete the root directory');
  if (Node.Count > 0) then
    raise Exception.Create('Cannot delete a directory that has subdirectories');
  Dir := Directory;
  Selected := Node.Parent;
  Current := GetCurrentDir;
  if Current[length(Current)] = '\' then System.delete(Current, length(Current), 1);
  if Dir[length(Dir)] = '\' then System.delete(Dir, length(Dir), 1);
  if Current = Dir then ChDir('..');
  RmDir(Dir);
  Node.Delete;
end;


procedure TDirectoryTree.Edited;
var Dir, NewDir: string;
begin
   Dir := GetDirectoryFor(node);
   System.Delete(Dir, Length(Dir), 1);
   NewDir := GetDirectoryFor(Node.Parent) + S;
   if not RenameFile(Dir, NewDir) then
        S := PDirectoryData(Node.Data).Name
   else
        PDirectoryData(Node.Data).Name := S + '\';
end;

procedure TDirectoryTree.AllowEditing;
begin
  if Node.Parent = nil then AllowEdit := False;
end;

procedure TDirectoryTree.Refresh;
var dir: string;
begin
  dir := Directory;
  items.Clear;
  InitDrives;
  Directory := dir;
end;

procedure Register;
begin
  RegisterComponents('MiLo', [TDirectoryTree]);
end;

end.
