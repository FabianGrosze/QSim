Programmstart  {#lnk_programmstart}
=============

Nach Abschluss der \ref lnk_modellerstellung kann das Programm QSim3D gestartet 
werden. Dazu gibt es die folgenden Möglichkeiten:

# Linux-Konsole {#lnk_programmstart_konsole}

Um lauffähig zu sein müssen QSim3D einige Environment Modules zur Verfügung
gestellt werden.  
Auf dem HPC der BfG lassen sich diese laden mit:

  * `module load produktiv/mvapich2-2.3-mlnx`
  * `module load produktiv/netcdf-fortran-4.4.2`
  * `module load produktiv/netcdf-4.5.0`

## Seriell {#lnk_programmstart_seriell}

Nachdem der Sourcecode von QSim3D kompiliert wurde und das daraus entstandene 
ausführbare Programm (Executable) z.B. in der Datei `/home/Wyrwa/bin/qsim3d` 
abgelegt wurde, kann es dann von der Konsole eines Linux-Rechners aus direkt 
ausgeführt werden, z.B.mit dem Kommando:

```
/home/Wyrwa/bin/qsim3d /srv/wyrwa/annu/ 769
```

Der erste, obligatorische Parameter ist das Modellverzeichnis
(mit abschließendem betriebssystemspezifischem Verzeichnistrenner).  
Der zweite, optionale Parameter ist eine Kontrollknotennummer für eine 
erweiterte Bildschirmausgabe.  
Das Programm wird damit seriell ausgeführt, d. h. alle Operationen werden 
nacheinander auf einem Prozessor ausgeführt.

## Parallel {#lnk_programmstart_parallel}

QSim3D ist mit dem Message Passing Interface (MPI) parallelisiert, um auf 
Parallelrechnern mit verteiltem Speicher ausführbar zu sein 
(siehe: \ref lnk_parallelisierung). Es wird dann das Kommando `mpiexec` verwendet, 
um QSim3D auf einer Gruppe von Prozessoren zu starten.

## Batch-queue {#lnk_batch-queue}

Teure Hochleistungsrechner wie der HPC (high-performance-cluster) der BfG 
stehen in der Regel nicht einzelnen Kollegen alleine zur Verfügung.
Um die Berechnungsaufträge von vielen Benutzern abzuarbeiten, werden sogenannte 
batch-queues (elektronische Warteschlangen) verwendet.

Der Benutzer übergibt seinen Berechnungsauftrag durch Start eines Skriptes an 
einen Scheduler; momentan wird slurm verwendet früher torque.

**Beispiel:** [qsim3d.sbatch](./exp/qsim3d.sbatch)  
Der Auftrag wird dann abgearbeitet, sobald die angeforderten Kapazitäten auf 
dem HPC verfügbar sind.
Slurm verfügt über Möglichkeiten den Benutzer über das Berechnungsende per Email 
zu informieren.

## Fortschritt {#lnk_sim_fortschritt}

Während des Rechenlaufs wird im Modellverzeichnis eine Datei `fortschritt`
abgelegt, die nur eine Zahl zwischen 0 und 1 enthält, und angibt, welcher Anteil 
der erforderlichen Zeitschritte bereits durchlaufen wurde.
Beim Beenden von QSim-3D wird diese Datei gelöscht. Das Vorhandensein von 
`fortschritt` verhindert, dass weitere
Rechenläufe gestartet werden, die dasselbe Modellverzeichnis verwenden. 

Bei unkontrollierten Programmabbrüchen wird `fortschritt` nicht gelöscht. 
Bei einem Neustart würde dies das Anlaufen von QSim3d verhindern;
d.h. `fortschritt` muss nach unkontrollierten Abbrüchen vor dem Neustart 
manuell entfernt werden.

# Benutzeroberfläche Gerris {#lnk_programmstart_gui}

Wenn bei der \ref lnk_gerris die Optionen richtig gesetzt wurden, lässt sich QSim3D 
durch Drücken auf den "Berechnen" Knopf starten:

\image html GERRISstart.png

Textquelle: programmstart.md ; Codesources:  QSim3D.f95 ;  
zurück: \ref lnk_modellbedienung oder \ref index
 