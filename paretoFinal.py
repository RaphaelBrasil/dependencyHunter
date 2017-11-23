## Gerador de Trafégo Pareto ON/OFF para a plataforma Hemps ##
##     Desenvolvedores: Lucas P. Lima, Raphael Brasil       ##
##           23/11/2017 IFCE - Campus Maracanaú             ##

import random
import string

##Inicialização##
AUX1 = int(input('Digite o número de Nós independetes da Aplicação: '))

PacketSendTime = 30
ETon = 50 * (10 ** (-3))
EToff = 40 * (10 ** (-3))
H = 0.75
##time = 0
SimulationTime = 10000

valoresON = []
valoresOFF = []

print(valoresON)


alphaOn = 3 - 2 * H

ro = ETon / (ETon + EToff)

alphaOff = ((1 - ro) * alphaOn) / (((1 - ro) * alphaOn) - (ro * (alphaOn - 1)))

print('AlphaOn: {}'.format(alphaOn))
print('AlphaOff: {}'.format(alphaOff))


for j in range (33):

    for x in range(AUX1):
        a = list(string.ascii_uppercase)
        time = 0
        AUX = 1
        while (time <= SimulationTime):
            U = random.uniform(0, 1)
            Pon = round(U ** (-1 / alphaOn))
            Poff = round(U ** (-1 / alphaOff))

            Ton = Pon * PacketSendTime
            Toff = Poff * PacketSendTime
            print('-----------------------')
            print('Inicio da Iteração {}{}'.format(a[x],AUX))
            print('Valor ON {}'.format(Ton))
            print('Valor OFF {}'.format(Toff))
            for i in range(Pon):
                print('Gera Pacote{}'.format(i+1))
                pass
            print('Tempo Ocioso: {}'.format(Toff))


            valoresON.append(Ton)
            valoresOFF.append(Toff)
            print(valoresON)
            print(valoresOFF)

            time = time + Ton + Toff
            AUX = AUX + 1
            if (AUX > 10):

                StrinOn = '{' + ','.join(str(e) for e in valoresON) + '}'
                StrinOff = '{' + ','.join(str(e) for e in valoresOFF) + '}'
                a = list(string.ascii_uppercase)
                arquivo = open('task{}{}.c'.format(a[x],j+1), 'w')
                arquivo.write('#include <api.h>\n')
                arquivo.write('#include <stdlib.h>\n')
                arquivo.write('#include "syn_std.h"\n')
                arquivo.write('Message msg;\n')
                arquivo.write('int main()\n')
                arquivo.write('{\n')
                arquivo.write(' int i, j,t;\n')
                arquivo.write(' int valoresON[SYNTHETIC_ITERATIONS] = {};\n'.format(StrinOn))
                arquivo.write(' int valoresOFF[SYNTHETIC_ITERATIONS] = {};\n'.format(StrinOff))
                arquivo.write(' Echo("synthetic task {} started.");\n'.format(a[x]))
                arquivo.write(' Echo(itoa(GetTick()));\n')
                arquivo.write(' for(i=0;i<SYNTHETIC_ITERATIONS;i++){\n')
                arquivo.write('     for(t=0;t<valoresOFF[i];t++){\n')
                arquivo.write('     }\n')
                arquivo.write('     msg.length = 30;\n')
                arquivo.write('     for(j=0;j<30;j++) msg.msg[j]=i;\n')
                arquivo.write('     for(b=0;b<valoresON[i];b++){\n')
                arquivo.write('         Send(&msg,taskC);\n')
                arquivo.write('     }\n')
                arquivo.write('}\n')
                arquivo.write('Echo(itoa(GetTick()));\n')
                arquivo.write('Echo("synthetic task {} finished.");\n'.format(a[x]))
                arquivo.write('exit();\n')
                arquivo.write('}\n')

                arquivo.close()
                valoresON = []
                valoresOFF = []

                break

            pass

        print('-----------------------')


