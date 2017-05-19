FROM ubuntu:16.04

#
# curl to install Google Cloud SDK.
# python is a required dependency of Google Cloud SDK. 
#
CMD apt-get -y update && apt-get -y install \
    curl
    python

CMD curl -sS https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-153.0.0-linux-x86_64.tar.gz | tar -xzf - -C /opt

# export CLOUDSDK_CORE_DISABLE_PROMPTS=1
# PATH=/opt/google-cloud-sdk/bin:$PATH

CMD gcloud components update && gcloud components install kubectl
