@echo off
setlocal

IF [%8]==[] "##gs##" %1 %2 %3 %4=%5 %6 %7

SET p1=%1
SET p2=%2
SET p3=%3
SET p4=%4
SET p5=%5
SET p6=%6
SET p7=%7
SET p8=%8
SET p9=%9
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SET p10=%1
SET p11=%2
SET p12=%3
SET p13=%4
SET p14=%5
SET p15=%6
SET p16=%7
SET p17=%8
SET p18=%9

IF NOT [%p8%]==[] "##gs##" %p1% %p2% %p3% %p4%=%p5% %p6% %p7%=%p8% %p9% %p10% %p11% %p12% %p13% %p14% %p15%
