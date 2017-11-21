from scipy.stats import pareto
import matplotlib.pyplot as plt

burst_time = 500
idle_time = 500
rate = 200
packetSize = 210
shape = 0.5

interval = (packetSize * 8) / rate
burstlen = burst_time / interval




for i in range(33):
  ##  b1 = (burstlen * (shape - 1)) / shape
  ##  b2 = (idle_time * (shape - 1)) / shape
  ##  print('B1: {} ||| B2: {}'.format(b1, b2))

    ##next_burtlen
    burstlen = int(pareto.pdf(i*-52, shape) + 0.5)
    if (burstlen == 0):
        burstlen = 1
    print('BurstLen {}: {}'.format(i, burstlen))

    ##next_idle_time
    idle_time= pareto.pdf(i*-22, shape)
    print('IdleTime {}: {}'.format(i, idle_time))
