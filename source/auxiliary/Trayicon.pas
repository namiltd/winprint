unit Trayicon;

interface

  uses
    SysUtils, Windows, Messages, Classes, Graphics, Controls, ShellAPI, Forms,
    Menus;

  const
    WM_TOOLTRAYICON = WM_USER+1;
    WM_RESETTOOLTIP = WM_USER+2;

  type
    TTrayIcon2 = class(TComponent)
    private
    // BDS
    { internal use }
      hMapping: THandle;
    { Field Variables }
      IconData: TNOTIFYICONDATA;
      fIcon : TIcon;
      fToolTip : String;
      fWindowHandle : HWND;
      fActive : boolean;
      fShowApp : boolean;                                     // A. Meeder
      fSendMsg : string;
      fShowDesigning : Boolean;
    { Events }
      fOnClick     : TNotifyEvent;
      fOnDblClick  : TNotifyEvent;
      fOnRightClick : TMouseEvent;
      fPopupMenu   : TPopupMenu;
      function AddIcon : boolean;
      function ModifyIcon : boolean;
      function DeleteIcon : boolean;
      procedure SetActive(Value : boolean);
      procedure SetShowApp(Value : boolean);                  // A. Meeder
      procedure SetShowDesigning(Value : boolean);
      procedure SetIcon(Value : TIcon);
      procedure SetToolTip(Value : String);
      procedure WndProc(var msg : TMessage);
      procedure FillDataStructure;
      procedure DoRightClick( Sender : TObject );
    protected
    public
      FMessageID: DWORD;
      constructor Create(aOwner : TComponent); override;
      procedure Loaded; override;                             // A. Meeder
      destructor Destroy; override;
      procedure GoToPreviousInstance;
    published
      property Active : boolean read fActive write SetActive;
      property ShowDesigning : boolean read fShowDesigning write SetShowDesigning;
      property Icon : TIcon read fIcon write SetIcon;
      property IDMessage : string read fSendMsg write fSendMsg;
      property ShowApp : boolean read fShowApp write SetShowApp; // A. Meeder
      property ToolTip : string read fTooltip write SetToolTip;
      property OnClick     : TNotifyEvent read FOnClick write FOnClick;
      property OnDblClick  : TNotifyEvent read FOnDblClick write FOnDblClick;
      property OnRightClick : TMouseEvent  read FOnRightClick write FonRightClick;
      property PopupMenu : TPopupMenu read fPopupMenu write fPopupMenu;
    end;

  procedure Register;

  type
     PHWND = ^HWND;


implementation

  // {$R TrayIcon.res}

  procedure TTrayIcon2.GoToPreviousInstance;
  begin
    PostMessage(hwnd_Broadcast, fMessageID, 0, 0);
  end;

  procedure TTrayIcon2.SetActive(Value : boolean);
  begin
     if value <> fActive then begin
       fActive := Value;
       if not (csdesigning in ComponentState) then begin
          if Value then begin
             AddIcon;
          end else begin
             DeleteIcon;
          end;
       end;
    end;
  end;

  procedure TTrayIcon2.SetShowApp(Value : boolean);      // A. Meeder
  begin
{    if value <> fShowApp then fShowApp := value;
    if not (csdesigning in ComponentState) then
    begin
      if Value then
      begin
        ShowWindow(Application.Handle, SW_SHOW);
      end
      else
      begin
        ShowWindow(Application.Handle, SW_HIDE);
      end;
    end;}
  end;

  procedure TTrayIcon2.SetShowDesigning(Value : boolean);
  begin
    if csdesigning in ComponentState then begin
       if value <> fShowDesigning then begin
          fShowDesigning := Value;
          if Value then begin
             AddIcon;
          end else begin
             DeleteIcon;
          end;
       end;
    end;
  end;

  procedure TTrayIcon2.SetIcon(Value : Ticon);
  begin
    if Value <> fIcon then
      begin
        fIcon.Assign(value);
        ModifyIcon;
      end;
  end;

  procedure TTrayIcon2.SetToolTip(Value : string);
  begin
     // This routine ALWAYS re-sets the field value and re-loads the
     // icon.  This is so the ToolTip can be set blank when the component
     // is first loaded.  If this is changed, the icon will be blank on
     // the tray when no ToolTip is specified.
     if length( Value ) > 62 then
        Value := copy(Value,1,62);
     fToolTip := value;
     ModifyIcon;
  end;

  constructor TTrayIcon2.create(aOwner : Tcomponent);
  begin
    inherited create(aOwner);
{$WARN SYMBOL_DEPRECATED OFF}
    FWindowHandle := AllocateHWnd( WndProc );
{$WARN SYMBOL_DEPRECATED ON}    
    FIcon := TIcon.Create;
    SetShowApp(False);
  end;

  destructor TTrayIcon2.destroy;
  begin
    // BDS
    CloseHandle(hMapping);

    if (not (csDesigning in ComponentState) and fActive)
       or ((csDesigning in ComponentState) and fShowDesigning) then
          DeleteIcon;
    FIcon.Free;
{$WARN SYMBOL_DEPRECATED OFF}
    DeAllocateHWnd( FWindowHandle );
{$WARN SYMBOL_DEPRECATED ON}    
    inherited destroy;
  end;

  procedure TTrayIcon2.Loaded;
  var
    // BDS
    // hMapping: HWND;
    tmp, tmpID: PChar;
  begin
    inherited Loaded;
    if fSendMsg <> '' then
    begin
      GetMem(tmp, Length(fSendMsg) + 1);
      GetMem(tmpID, Length(fSendMsg) + 1);
      StrPCopy(tmp, fSendMsg);
      StrPCopy(tmpID, fSendMsg);
      fMessageID := RegisterWindowMessage(tmp);
      FreeMem(tmp);
      hMapping := CreateFileMapping(HWND($FFFFFFFF), nil, PAGE_READONLY, 0, 32, tmpID);
      if (hMapping <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then
      begin
        if not (csDesigning in ComponentState) then
        begin
          GotoPreviousInstance;
          FreeMem(tmpID);
          halt;
        end;
      end;
      FreeMem(tmpID);
    end;
    SetShowApp(fShowApp);
  end;

  procedure TTrayIcon2.FillDataStructure;
  begin
    with IconData do begin
       cbSize := sizeof(TNOTIFYICONDATA);
       wnd := FWindowHandle;
       uID := 0; // is not passed in with message so make it 0
       uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
       hIcon := fIcon.Handle;
       StrPCopy(szTip,fToolTip);
       uCallbackMessage := WM_TOOLTRAYICON;
    end;
  end;

  function TTrayIcon2.AddIcon : boolean;
  begin
     FillDataStructure;
     result := Shell_NotifyIcon(NIM_ADD,@IconData);
     // For some reason, if there is no tool tip set up, then the icon
     // doesn't display.  This fixes that.
     if fToolTip = '' then
        PostMessage( fWindowHandle, WM_RESETTOOLTIP,0,0 );
  end;

  function TTrayIcon2.ModifyIcon : boolean;
  begin
     FillDataStructure;
     if fActive then
        result := Shell_NotifyIcon(NIM_MODIFY,@IconData)
     else
        result := True;
  end;

  procedure TTrayIcon2.DoRightClick( Sender : TObject );
  var MouseCo: Tpoint;
  begin
     GetCursorPos(MouseCo);
     if assigned( fPopupMenu ) then begin
        SetForegroundWindow( Application.Handle );
        Application.ProcessMessages;
        fPopupmenu.Popup( Mouseco.X, Mouseco.Y );
     end;
     if assigned( FOnRightClick ) then
        begin
           FOnRightClick(self,mbRight,[],MouseCo.x,MouseCo.y);
        end;
  end;

  function TTrayIcon2.DeleteIcon : boolean;
  begin
     result := Shell_NotifyIcon(NIM_DELETE,@IconData);
  end;

  procedure TTrayIcon2.WndProc(var msg : TMessage);
  begin
     with msg do
       if (msg = WM_RESETTOOLTIP) then
          SetToolTip( fToolTip )
       else if (msg = WM_TOOLTRAYICON) then begin
          case lParam of
             WM_LBUTTONDBLCLK   : if assigned (FOnDblClick) then FOnDblClick(self);
             WM_LBUTTONUP       : if assigned(FOnClick)then FOnClick(self);
             WM_RBUTTONUP       : DoRightClick(self);
          end;
       end
       else // Handle all messages with the default handler
          Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
  end;

  procedure Register;
  begin
    RegisterComponents('MyComponents', [TTrayIcon2]);
  end;

end.




