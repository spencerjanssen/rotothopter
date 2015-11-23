let region = "us-west-2";
    accessKeyId = "nixops";
    ec2_machine =
        { resources, ... }:
        {
            deployment.targetEnv = "ec2";
            deployment.ec2.region = region;
            deployment.ec2.instanceType = "t2.micro";
            deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
            deployment.ec2.accessKeyId = "nixops";
            deployment.ec2.securityGroups = [ "allow-ssh" "allow-http" ];
            deployment.ec2.elasticIPv4 = resources.elasticIPs.public;
        };
in {
    webserver = ec2_machine;
    resources.ec2KeyPairs.my-key-pair = {inherit region accessKeyId; };
    resources.elasticIPs.public = {inherit region accessKeyId; };
}
