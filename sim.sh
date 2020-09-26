cd /home/acooke/Sims/
sshpass -p "password" ssh -XY cooke@localhost -p port_number "mkdir ~cluster/sim_repo/"
#sshpass -p "password" ssh -XY cooke@localhost -p port_number "cp ~cluster/test.sh ~cluster/sim_repo/"
sshpass -p "password" ssh -XY cooke@localhost -p port_number "cp ~cluster/gravity.conf ~cluster/sim_repo/"
sshpass -p "password" ssh -XY cooke@localhost -p port_number "cp ~cluster/gravity.txt ~cluster/sim_repo/"
sshpass -p "password" ssh -XY cooke@localhost -p port_number "cp ~cluster/test.in ~cluster/sim_repo/"
for f in *.h5 ; do
sshpass -p "password" scp -P port_number /home/acooke/Sims/$f cooke@localhost:~cluster/sim_repo;
sshpass -p "password" ssh -XY cooke@localhost -p port_number "sh ~cluster/sim_repo/calc_grav_cluster.sh";
done; 
