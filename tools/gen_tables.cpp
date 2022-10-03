#include <iostream>
#include <string>
#include <windows.h>

using namespace std;

int main(int argc, char **argv)
{

   char x = 176;
   unsigned int cpin;
   unsigned int cpinnew;
   if (argc>1) {
      cpin = atoi(argv[1]);
   } else {
       cpin = 852;
   }
   if (cpin==808) {
       cpinnew=866;
//   } else if (cpin==858) {
//       cpinnew=850;
   } else if (cpin==867) {
       cpinnew=862;
   } else if (cpin==872) {
       cpinnew=855;     
   } else {
       cpinnew=cpin;
   }
   unsigned int cpout = 65001;
   cout <<  " CP" << cpin << "toUTF8: TUTF8Table=(";
   for(short int i=128;i<256;i++) {
       x = i;
       int chars_num = 4;
       string strTo( chars_num, 0 );
       if (((cpin==808)&&(i==253))
            ||((cpin==872)&&(i==207))
            //||((cpin==858)&&(i==213))
            ) {
              chars_num = 3; strTo[0]=226; strTo[1]=130; strTo[2]=172;
        } else if ((cpin==867)&&(i==159)) { chars_num = 3; strTo[0]=226; strTo[1]=130; strTo[2]=170;
        } else if ((cpin==867)&&(i==160)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=142;
        } else if ((cpin==867)&&(i==161)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=143;
        } else if ((cpin==867)&&(i==162)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=170;
        } else if ((cpin==867)&&(i==163)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=171;
        } else if ((cpin==867)&&(i==164)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=173;
        } else if ((cpin==867)&&(i==165)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=174;
        } else if ((cpin==867)&&(i==166)) { chars_num = 3; strTo[0]=226; strTo[1]=128; strTo[2]=172;
        } else if ((cpin==867)&&(i==173)) { chars_num = 3; strTo[0]=226; strTo[1]=130; strTo[2]=172;
        } else {
           int wchars_num = MultiByteToWideChar( cpinnew , 0 , &x , 1, NULL , 0 );
           wchar_t* wstr = new wchar_t[wchars_num];
           MultiByteToWideChar( cpinnew , 0 , &x , 1, wstr , wchars_num );
           chars_num = WideCharToMultiByte( cpout, 0 , &wstr[0] , wchars_num , NULL, 0, NULL , NULL);
//           string strTo( chars_num, 0 );
           WideCharToMultiByte( cpout, 0 , &wstr[0] , wchars_num, &strTo[0], chars_num, NULL, NULL);
           delete[] wstr;
       }
       if (!(i%4)) cout << "\n   ";
       cout << "(";
       for(short int j=0;j<chars_num;j++) {
           if (j>0) cout << "+";
           cout << "#$";
           cout << uppercase << hex << (int)(unsigned char)strTo[j];
       }
       strTo.clear();
       if (i<255) cout << "),";
       else cout << ")";
    }
    cout << ");\n\n";

    return 0;
}
