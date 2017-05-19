
Requires a k8s secret to be created something like:

```shell-session
$ kubectl --namespace=staging create secret generic webhook --from-literal=webhookSecret=put-your-webhook-secret-here
```
