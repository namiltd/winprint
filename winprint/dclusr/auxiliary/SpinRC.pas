unit SpinRC;

interface

uses Windows, Classes;

procedure Register;

implementation

uses Spin;

procedure Register;
begin
  RegisterComponents('MyComponents', [TSpinEdit,TSpinButton,TTimerSpeedButton]);
end;

end.
