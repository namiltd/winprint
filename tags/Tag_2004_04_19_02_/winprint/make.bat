@echo off

rem {******************************************************************************}
rem {                                                                              }
rem {   WinPrint - Print Spooler for DOS Programs                                  }
rem {                                                                              }
rem {   Copyright (C) 2004 Przemyslaw Czerkas <przemekc@users.sourceforge.net>     }
rem {   See GPL.TXT for copyright and license details.                             }
rem {                                                                              }
rem {******************************************************************************}
rem 
rem {******************************************************************************}
rem {                                                                              }
rem {   This file is part of WinPrint.                                             }
rem {                                                                              }
rem {   WinPrint is free software; you can redistribute it and/or modify           }
rem {   it under the terms of the GNU General Public License as published by       }
rem {   the Free Software Foundation; either version 2 of the License, or          }
rem {   (at your option) any later version.                                        }
rem {                                                                              }
rem {   WinPrint is distributed in the hope that it will be useful,                }
rem {   but WITHOUT ANY WARRANTY; without even the implied warranty of             }
rem {   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              }
rem {   GNU General Public License for more details.                               }
rem {                                                                              }
rem {   You should have received a copy of the GNU General Public License          }
rem {   along with WinPrint; if not, write to the Free Software                    }
rem {   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                      }
rem {   MA  02111-1307  USA                                                        }
rem {                                                                              }
rem {******************************************************************************}

cd resources\
call make.bat
copy icons32x32\icons.res ..\icons.res
cd ..\

dcc32 -B WinPrint.dpr 

del icons.res
