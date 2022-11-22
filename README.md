### MEMO

Assignment_for digital twin class.

This DT is built by Erlang server.





APIs to interact with the dt plant:
```
http://localhost:8080/dtplant?echo=humidity

```


```
git pull
git add .
git commit -m "Message want to add"
git push origin master
```


---



yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker ps
CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS    PORTS     NAMES
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker images
REPOSITORY               TAG       IMAGE ID       CREATED              SIZE
hyc0812/yong-dt-erlang   1.0       33b352d9c7b4   About a minute ago   17.3MB
hyc0812/yong_dt          1.2       4e8eb243138e   6 days ago           436MB
hyc0812/yong_dt          1.1       338e359ecf42   6 days ago           436MB
mysql                    latest    2a04bf34fdf0   2 weeks ago          535MB
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker run --name erlang_dt dp 8080:8080 hyc0812/yong-dt-erlang:1.0
Unable to find image 'dp:latest' locally
docker: Error response from daemon: pull access denied for dp, repository does not exist or may require 'docker login': denied: requested access to the resource is denied.
See 'docker run --help'.
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker run --name erlang_dt -dp 8080:8080 hyc0812/yong-dt-erlang:1.0
d31ed2b904b4488a79274e84b509b7bb7c61673dfa76118560d46c8c51c16d74
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docke ps
zsh: command not found: docke
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker ps
CONTAINER ID   IMAGE                        COMMAND                  CREATED         STATUS         PORTS                              NAMES
d31ed2b904b4   hyc0812/yong-dt-erlang:1.0   "/docker_ex/bin/dock…"   7 seconds ago   Up 6 seconds   0.0.0.0:8080->8080/tcp, 8443/tcp   erlang_dt
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker stop d31ed2b904b4
d31ed2b904b4
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker ps
CONTAINER ID   IMAGE                  COMMAND                  CREATED          STATUS          PORTS     NAMES
605f230f4f05   1c7d8c51823b           "/usr/local/bin/kube…"   5 seconds ago    Up 4 seconds              k8s_kube-proxy_kube-proxy-llk4m_kube-system_570a534e-bfe3-4523-830b-4ab244d7b60b_0
bafed4baaf77   k8s.gcr.io/pause:3.8   "/pause"                 5 seconds ago    Up 4 seconds              k8s_POD_kube-proxy-llk4m_kube-system_570a534e-bfe3-4523-830b-4ab244d7b60b_0
af08d3e3fcf9   ca0ea1ee3cfd           "kube-scheduler --au…"   16 seconds ago   Up 15 seconds             k8s_kube-scheduler_kube-scheduler-docker-desktop_kube-system_3744c28618b9eefc6c47dfb0a45744a6_0
cdaab3048383   a8a176a5d5d6           "etcd --advertise-cl…"   16 seconds ago   Up 15 seconds             k8s_etcd_etcd-docker-desktop_kube-system_c4a48fe4cae9bb9e6d7e065357601b3f_0
69914f7843b7   97801f839490           "kube-apiserver --ad…"   16 seconds ago   Up 15 seconds             k8s_kube-apiserver_kube-apiserver-docker-desktop_kube-system_8e9132c31407bb3ec5eabb4d9d72cbf3_0
229006c14e2a   dbfceb93c69b           "kube-controller-man…"   16 seconds ago   Up 15 seconds             k8s_kube-controller-manager_kube-controller-manager-docker-desktop_kube-system_6c75172049c399028f4c1d6e23f5dbc7_0
9defd6dbaa2d   k8s.gcr.io/pause:3.8   "/pause"                 17 seconds ago   Up 16 seconds             k8s_POD_kube-controller-manager-docker-desktop_kube-system_6c75172049c399028f4c1d6e23f5dbc7_0
37e9f85d2ef9   k8s.gcr.io/pause:3.8   "/pause"                 17 seconds ago   Up 16 seconds             k8s_POD_kube-apiserver-docker-desktop_kube-system_8e9132c31407bb3ec5eabb4d9d72cbf3_0
ecd6369955f8   k8s.gcr.io/pause:3.8   "/pause"                 17 seconds ago   Up 16 seconds             k8s_POD_etcd-docker-desktop_kube-system_c4a48fe4cae9bb9e6d7e065357601b3f_0
9e338af285de   k8s.gcr.io/pause:3.8   "/pause"                 17 seconds ago   Up 16 seconds             k8s_POD_kube-scheduler-docker-desktop_kube-system_3744c28618b9eefc6c47dfb0a45744a6_0
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % ls
Dockerfile	LICENSE		README.md	pod.yaml	rebar.config	rebar.lock	src
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker images
REPOSITORY                                                TAG                                                                          IMAGE ID       CREATED          SIZE
hyc0812/yong-dt-erlang                                    1.0                                                                          33b352d9c7b4   29 minutes ago   17.3MB
hyc0812/yong_dt                                           1.2                                                                          4e8eb243138e   6 days ago       436MB
hyc0812/yong_dt                                           1.1                                                                          338e359ecf42   6 days ago       436MB
mysql                                                     latest                                                                       2a04bf34fdf0   2 weeks ago      535MB
hubproxy.docker.internal:5000/docker/desktop-kubernetes   kubernetes-v1.25.2-cni-v1.1.1-critools-v1.24.2-cri-dockerd-v0.2.5-1-debian   09d7e1dbc2c4   8 weeks ago      363MB
k8s.gcr.io/kube-apiserver                                 v1.25.2                                                                      97801f839490   2 months ago     128MB
k8s.gcr.io/kube-controller-manager                        v1.25.2                                                                      dbfceb93c69b   2 months ago     117MB
k8s.gcr.io/kube-scheduler                                 v1.25.2                                                                      ca0ea1ee3cfd   2 months ago     50.6MB
k8s.gcr.io/kube-proxy                                     v1.25.2                                                                      1c7d8c51823b   2 months ago     61.7MB
k8s.gcr.io/pause                                          3.8                                                                          4873874c08ef   5 months ago     711kB
k8s.gcr.io/etcd                                           3.5.4-0                                                                      a8a176a5d5d6   5 months ago     300MB
k8s.gcr.io/coredns                                        v1.9.3                                                                       5185b96f0bec   5 months ago     48.8MB
docker/desktop-vpnkit-controller                          v2.0                                                                         8c2c38aa676e   18 months ago    21MB
docker/desktop-storage-provisioner                        v2.0                                                                         99f89471f470   19 months ago    41.9MB
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % touch pod.yaml
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % vi pod.yaml 
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % ls
Dockerfile	LICENSE		README.md	pod.yaml	rebar.config	rebar.lock	src
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl apply -f pod.yaml
deployment.apps/myapp-deployment created
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           14s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           45s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           53s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           59s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           61s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           63s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           64s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           80s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           92s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           2m40s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker images
REPOSITORY                                                TAG                                                                          IMAGE ID       CREATED          SIZE
hyc0812/yong-dt-erlang                                    1.0                                                                          33b352d9c7b4   39 minutes ago   17.3MB
hyc0812/yong_dt                                           1.2                                                                          4e8eb243138e   6 days ago       436MB
hyc0812/yong_dt                                           1.1                                                                          338e359ecf42   6 days ago       436MB
mysql                                                     latest                                                                       2a04bf34fdf0   2 weeks ago      535MB
hubproxy.docker.internal:5000/docker/desktop-kubernetes   kubernetes-v1.25.2-cni-v1.1.1-critools-v1.24.2-cri-dockerd-v0.2.5-1-debian   09d7e1dbc2c4   8 weeks ago      363MB
k8s.gcr.io/kube-apiserver                                 v1.25.2                                                                      97801f839490   2 months ago     128MB
k8s.gcr.io/kube-scheduler                                 v1.25.2                                                                      ca0ea1ee3cfd   2 months ago     50.6MB
k8s.gcr.io/kube-controller-manager                        v1.25.2                                                                      dbfceb93c69b   2 months ago     117MB
k8s.gcr.io/kube-proxy                                     v1.25.2                                                                      1c7d8c51823b   2 months ago     61.7MB
k8s.gcr.io/pause                                          3.8                                                                          4873874c08ef   5 months ago     711kB
k8s.gcr.io/etcd                                           3.5.4-0                                                                      a8a176a5d5d6   5 months ago     300MB
k8s.gcr.io/coredns                                        v1.9.3                                                                       5185b96f0bec   5 months ago     48.8MB
docker/desktop-vpnkit-controller                          v2.0                                                                         8c2c38aa676e   18 months ago    21MB
docker/desktop-storage-provisioner                        v2.0                                                                         99f89471f470   19 months ago    41.9MB
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % docker push hyc0812/yong-dt-erlang:1.0
The push refers to repository [docker.io/hyc0812/yong-dt-erlang]
10915ef1e106: Pushed 
ac6ff7601d7d: Pushed 
e5e13b0c77cb: Mounted from hyc0812/yong_dt 
1.0: digest: sha256:17f2d97cca1eeaed37db3a225af166be886ab16fecdd6439b56196ba76114eb6 size: 950
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments               
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           3m38s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl apply -f pod.yaml             
deployment.apps/myapp-deployment unchanged
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments                
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           4m11s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments               
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           4m22s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get pods       
NAME                                READY   STATUS             RESTARTS   AGE
myapp-deployment-7977dcc558-74qss   0/1     ImagePullBackOff   0          4m28s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl describe pod myapp-deployment-7977dcc558-74qss
Name:             myapp-deployment-7977dcc558-74qss
Namespace:        default
Priority:         0
Service Account:  default
Node:             docker-desktop/192.168.65.4
Start Time:       Mon, 21 Nov 2022 20:54:20 -0600
Labels:           app=myapp
                  pod-template-hash=7977dcc558
Annotations:      <none>
Status:           Pending
IP:               10.1.0.6
IPs:
  IP:           10.1.0.6
Controlled By:  ReplicaSet/myapp-deployment-7977dcc558
Containers:
  myapp:
    Container ID:   
    Image:          hyc0812/yong-dt-erlang:v1.0
    Image ID:       
    Port:           <none>
    Host Port:      <none>
    State:          Waiting
      Reason:       ImagePullBackOff
    Ready:          False
    Restart Count:  0
    Environment:    <none>
    Mounts:
      /var/run/secrets/kubernetes.io/serviceaccount from kube-api-access-m5sgn (ro)
Conditions:
  Type              Status
  Initialized       True 
  Ready             False 
  ContainersReady   False 
  PodScheduled      True 
Volumes:
  kube-api-access-m5sgn:
    Type:                    Projected (a volume that contains injected data from multiple sources)
    TokenExpirationSeconds:  3607
    ConfigMapName:           kube-root-ca.crt
    ConfigMapOptional:       <nil>
    DownwardAPI:             true
QoS Class:                   BestEffort
Node-Selectors:              <none>
Tolerations:                 node.kubernetes.io/not-ready:NoExecute op=Exists for 300s
                             node.kubernetes.io/unreachable:NoExecute op=Exists for 300s
Events:
  Type     Reason     Age                    From               Message
  ----     ------     ----                   ----               -------
  Normal   Scheduled  6m34s                  default-scheduler  Successfully assigned default/myapp-deployment-7977dcc558-74qss to docker-desktop
  Normal   Pulling    5m13s (x4 over 6m33s)  kubelet            Pulling image "hyc0812/yong-dt-erlang:v1.0"
  Warning  Failed     5m12s (x4 over 6m32s)  kubelet            Failed to pull image "hyc0812/yong-dt-erlang:v1.0": rpc error: code = Unknown desc = Error response from daemon: pull access denied for hyc0812/yong-dt-erlang, repository does not exist or may require 'docker login': denied: requested access to the resource is denied
  Warning  Failed     5m12s (x4 over 6m32s)  kubelet            Error: ErrImagePull
  Warning  Failed     4m46s (x6 over 6m31s)  kubelet            Error: ImagePullBackOff
  Normal   BackOff    81s (x20 over 6m31s)   kubelet            Back-off pulling image "hyc0812/yong-dt-erlang:v1.0"
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments                               
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           7m54s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl apply -f pod.yaml                             
deployment.apps/myapp-deployment unchanged
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl apply -f pod.yaml
deployment.apps/myapp-deployment created
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments  
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           4s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments  
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           8s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           10s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           17s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   0/1     1            0           23s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get pods       
NAME                                READY   STATUS             RESTARTS   AGE
myapp-deployment-7977dcc558-t98hq   0/1     ImagePullBackOff   0          32s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get pods
NAME                                READY   STATUS         RESTARTS   AGE
myapp-deployment-7977dcc558-t98hq   0/1     ErrImagePull   0          61s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get pods
NAME                                READY   STATUS         RESTARTS   AGE
myapp-deployment-7977dcc558-t98hq   0/1     ErrImagePull   0          67s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl describe pod myapp-deployment-7977dcc558-t98hq
Name:             myapp-deployment-7977dcc558-t98hq
Namespace:        default
Priority:         0
Service Account:  default
Node:             docker-desktop/192.168.65.4
Start Time:       Mon, 21 Nov 2022 21:04:28 -0600
Labels:           app=myapp
                  pod-template-hash=7977dcc558
Annotations:      <none>
Status:           Pending
IP:               10.1.0.11
IPs:
  IP:           10.1.0.11
Controlled By:  ReplicaSet/myapp-deployment-7977dcc558
Containers:
  myapp:
    Container ID:   
    Image:          hyc0812/yong-dt-erlang:v1.0
    Image ID:       
    Port:           <none>
    Host Port:      <none>
    State:          Waiting
      Reason:       ImagePullBackOff
    Ready:          False
    Restart Count:  0
    Environment:    <none>
    Mounts:
      /var/run/secrets/kubernetes.io/serviceaccount from kube-api-access-v6pwg (ro)
Conditions:
  Type              Status
  Initialized       True 
  Ready             False 
  ContainersReady   False 
  PodScheduled      True 
Volumes:
  kube-api-access-v6pwg:
    Type:                    Projected (a volume that contains injected data from multiple sources)
    TokenExpirationSeconds:  3607
    ConfigMapName:           kube-root-ca.crt
    ConfigMapOptional:       <nil>
    DownwardAPI:             true
QoS Class:                   BestEffort
Node-Selectors:              <none>
Tolerations:                 node.kubernetes.io/not-ready:NoExecute op=Exists for 300s
                             node.kubernetes.io/unreachable:NoExecute op=Exists for 300s
Events:
  Type     Reason     Age                  From               Message
  ----     ------     ----                 ----               -------
  Normal   Scheduled  2m10s                default-scheduler  Successfully assigned default/myapp-deployment-7977dcc558-t98hq to docker-desktop
  Normal   Pulling    36s (x4 over 2m10s)  kubelet            Pulling image "hyc0812/yong-dt-erlang:v1.0"
  Warning  Failed     35s (x4 over 2m9s)   kubelet            Failed to pull image "hyc0812/yong-dt-erlang:v1.0": rpc error: code = Unknown desc = Error response from daemon: manifest for hyc0812/yong-dt-erlang:v1.0 not found: manifest unknown: manifest unknown
  Warning  Failed     35s (x4 over 2m9s)   kubelet            Error: ErrImagePull
  Warning  Failed     21s (x6 over 2m8s)   kubelet            Error: ImagePullBackOff
  Normal   BackOff    6s (x7 over 2m8s)    kubelet            Back-off pulling image "hyc0812/yong-dt-erlang:v1.0"
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % ls                                    
Dockerfile	LICENSE		README.md	pod.yaml	rebar.config	rebar.lock	src
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % vi pod.yaml 
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl apply -f pod.yaml                             
deployment.apps/myapp-deployment configured
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get pods                                      
NAME                               READY   STATUS    RESTARTS   AGE
myapp-deployment-d9c695dc6-6c8kx   1/1     Running   0          7s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl get deployments
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   1/1     1            1           6m1s
yongchanghe@MacBookPro deploy_docker_dtplant_erlang % kubectl port-forward myapp-deployment-d9c695dc6-6c8kx 8080:8080
Forwarding from 127.0.0.1:8080 -> 8080
Forwarding from [::1]:8080 -> 8080
Handling connection for 8080
Handling connection for 8080
Handling connection for 8080
Handling connection for 8080
^C%   




---


Last login: Mon Nov 21 20:14:13 on ttys002
yongchanghe@MacBookPro ~ % docker ps
CONTAINER ID   IMAGE                    COMMAND                  CREATED          STATUS          PORTS     NAMES
8089aa815d7a   hyc0812/yong-dt-erlang   "/docker_ex/bin/dock…"   3 minutes ago    Up 3 minutes              k8s_myapp_myapp-deployment-d9c695dc6-6c8kx_default_304e88e3-e7bb-41ae-a5a3-1c74de6fa273_0
da9b01e48ed2   k8s.gcr.io/pause:3.8     "/pause"                 3 minutes ago    Up 3 minutes              k8s_POD_myapp-deployment-d9c695dc6-6c8kx_default_304e88e3-e7bb-41ae-a5a3-1c74de6fa273_0
08ab633b0a59   8c2c38aa676e             "/kube-vpnkit-forwar…"   9 minutes ago    Up 9 minutes              k8s_vpnkit-controller_vpnkit-controller_kube-system_537e5d6e-e125-4e59-8ebc-3ff0be43bc91_0
3abfa799cce9   99f89471f470             "/storage-provisione…"   9 minutes ago    Up 9 minutes              k8s_storage-provisioner_storage-provisioner_kube-system_2dd56bbf-3f46-4a6b-998f-03561de3f8ac_0
a64190cf33e6   k8s.gcr.io/pause:3.8     "/pause"                 9 minutes ago    Up 9 minutes              k8s_POD_vpnkit-controller_kube-system_537e5d6e-e125-4e59-8ebc-3ff0be43bc91_0
3648f72db488   k8s.gcr.io/pause:3.8     "/pause"                 9 minutes ago    Up 9 minutes              k8s_POD_storage-provisioner_kube-system_2dd56bbf-3f46-4a6b-998f-03561de3f8ac_0
1d246b90f4d4   5185b96f0bec             "/coredns -conf /etc…"   9 minutes ago    Up 9 minutes              k8s_coredns_coredns-95db45d46-62c45_kube-system_71b8e423-37b4-401c-bff4-549c331ef0d7_0
a8a8f2c22ef3   5185b96f0bec             "/coredns -conf /etc…"   9 minutes ago    Up 9 minutes              k8s_coredns_coredns-95db45d46-8znnn_kube-system_5eafcff9-cd4f-4687-94ed-75a1472732ff_0
f51b08844132   k8s.gcr.io/pause:3.8     "/pause"                 9 minutes ago    Up 9 minutes              k8s_POD_coredns-95db45d46-8znnn_kube-system_5eafcff9-cd4f-4687-94ed-75a1472732ff_0
1c862e013533   k8s.gcr.io/pause:3.8     "/pause"                 9 minutes ago    Up 9 minutes              k8s_POD_coredns-95db45d46-62c45_kube-system_71b8e423-37b4-401c-bff4-549c331ef0d7_0
46f406b15d91   1c7d8c51823b             "/usr/local/bin/kube…"   10 minutes ago   Up 10 minutes             k8s_kube-proxy_kube-proxy-tzc8j_kube-system_5fae1275-bc78-4ab1-9dd3-46e4db941264_0
174fadd3183a   k8s.gcr.io/pause:3.8     "/pause"                 10 minutes ago   Up 10 minutes             k8s_POD_kube-proxy-tzc8j_kube-system_5fae1275-bc78-4ab1-9dd3-46e4db941264_0
8144056fba4d   97801f839490             "kube-apiserver --ad…"   10 minutes ago   Up 10 minutes             k8s_kube-apiserver_kube-apiserver-docker-desktop_kube-system_8e9132c31407bb3ec5eabb4d9d72cbf3_1
7b1b264a476a   dbfceb93c69b             "kube-controller-man…"   10 minutes ago   Up 10 minutes             k8s_kube-controller-manager_kube-controller-manager-docker-desktop_kube-system_6c75172049c399028f4c1d6e23f5dbc7_1
6ed1222cde86   a8a176a5d5d6             "etcd --advertise-cl…"   10 minutes ago   Up 10 minutes             k8s_etcd_etcd-docker-desktop_kube-system_c4a48fe4cae9bb9e6d7e065357601b3f_1
1689ac4880ac   ca0ea1ee3cfd             "kube-scheduler --au…"   10 minutes ago   Up 10 minutes             k8s_kube-scheduler_kube-scheduler-docker-desktop_kube-system_3744c28618b9eefc6c47dfb0a45744a6_1
028f55cda942   k8s.gcr.io/pause:3.8     "/pause"                 10 minutes ago   Up 10 minutes             k8s_POD_kube-controller-manager-docker-desktop_kube-system_6c75172049c399028f4c1d6e23f5dbc7_0
dffbe222bb18   k8s.gcr.io/pause:3.8     "/pause"                 10 minutes ago   Up 10 minutes             k8s_POD_kube-apiserver-docker-desktop_kube-system_8e9132c31407bb3ec5eabb4d9d72cbf3_0
f191a7cfffc4   k8s.gcr.io/pause:3.8     "/pause"                 10 minutes ago   Up 10 minutes             k8s_POD_etcd-docker-desktop_kube-system_c4a48fe4cae9bb9e6d7e065357601b3f_0
3516afcc83be   k8s.gcr.io/pause:3.8     "/pause"                 10 minutes ago   Up 10 minutes             k8s_POD_kube-scheduler-docker-desktop_kube-system_3744c28618b9eefc6c47dfb0a45744a6_0
yongchanghe@MacBookPro ~ % kubectl scale deployment myapp-deployment --replicas=3
deployment.apps/myapp-deployment scaled
yongchanghe@MacBookPro ~ % kubectl get deployments                               
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
myapp-deployment   3/3     3            3           12m
yongchanghe@MacBookPro ~ % kubecel get pods      
zsh: command not found: kubecel
yongchanghe@MacBookPro ~ % kubectl get pods
NAME                               READY   STATUS    RESTARTS   AGE
myapp-deployment-d9c695dc6-6c8kx   1/1     Running   0          6m54s
myapp-deployment-d9c695dc6-tmgph   1/1     Running   0          41s
myapp-deployment-d9c695dc6-x6xps   1/1     Running   0          41s
yongchanghe@MacBookPro ~ % kubectl expose deployment myapp-deployment --type=LoadBalancer
error: couldn't find port via --port flag or introspection
yongchanghe@MacBookPro ~ % kubectl expose deployment myapp-deployment --type=LoadBalancer --port=8090
service/myapp-deployment exposed
yongchanghe@MacBookPro ~ % kubectl get pods        
NAME                               READY   STATUS    RESTARTS   AGE
myapp-deployment-d9c695dc6-6c8kx   1/1     Running   0          14m
myapp-deployment-d9c695dc6-tmgph   1/1     Running   0          8m3s
myapp-deployment-d9c695dc6-x6xps   1/1     Running   0          8m3s
yongchanghe@MacBookPro ~ % kubectl describe pods myapp-deployment-d9c695dc6-6c8kx                    
Name:             myapp-deployment-d9c695dc6-6c8kx
Namespace:        default
Priority:         0
Service Account:  default
Node:             docker-desktop/192.168.65.4
Start Time:       Mon, 21 Nov 2022 21:10:07 -0600
Labels:           app=myapp
                  pod-template-hash=d9c695dc6
Annotations:      <none>
Status:           Running
IP:               10.1.0.12
IPs:
  IP:           10.1.0.12
Controlled By:  ReplicaSet/myapp-deployment-d9c695dc6
Containers:
  myapp:
    Container ID:   docker://8089aa815d7a486676e5ba30528a6e771bcbbd3815e78f1f36d00e310b68056d
    Image:          hyc0812/yong-dt-erlang:1.0
    Image ID:       docker-pullable://hyc0812/yong-dt-erlang@sha256:17f2d97cca1eeaed37db3a225af166be886ab16fecdd6439b56196ba76114eb6
    Port:           <none>
    Host Port:      <none>
    State:          Running
      Started:      Mon, 21 Nov 2022 21:10:09 -0600
    Ready:          True
    Restart Count:  0
    Environment:    <none>
    Mounts:
      /var/run/secrets/kubernetes.io/serviceaccount from kube-api-access-prpcb (ro)
Conditions:
  Type              Status
  Initialized       True 
  Ready             True 
  ContainersReady   True 
  PodScheduled      True 
Volumes:
  kube-api-access-prpcb:
    Type:                    Projected (a volume that contains injected data from multiple sources)
    TokenExpirationSeconds:  3607
    ConfigMapName:           kube-root-ca.crt
    ConfigMapOptional:       <nil>
    DownwardAPI:             true
QoS Class:                   BestEffort
Node-Selectors:              <none>
Tolerations:                 node.kubernetes.io/not-ready:NoExecute op=Exists for 300s
                             node.kubernetes.io/unreachable:NoExecute op=Exists for 300s
Events:
  Type    Reason     Age   From               Message
  ----    ------     ----  ----               -------
  Normal  Scheduled  14m   default-scheduler  Successfully assigned default/myapp-deployment-d9c695dc6-6c8kx to docker-desktop
  Normal  Pulling    14m   kubelet            Pulling image "hyc0812/yong-dt-erlang:1.0"
  Normal  Pulled     14m   kubelet            Successfully pulled image "hyc0812/yong-dt-erlang:1.0" in 1.088331017s
  Normal  Created    14m   kubelet            Created container myapp
  Normal  Started    14m   kubelet            Started container myapp
yongchanghe@MacBookPro ~ % 
