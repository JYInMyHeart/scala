package jawa;

import java.util.*;

public class PaxosDemo {
    private static Random RANDOM = new Random();
    private static final String[] PROPOSALS = {"ProjectA", "ProjectB", "ProjectC"};
    private static void printInfo(String subject, String operation, String result) {
        System.out.println(subject + ":" + operation + "<" + result + ">");
    }

    public static void main(String[] args) {
        List<Acceptor> acceptors = new ArrayList<>();
        Arrays.asList("A", "B", "C", "D", "E")
                .forEach(name -> acceptors.add(new Acceptor(name)));
        Proposer.vote(new Proposal(1, null), acceptors);
    }


    private static Proposal nextProposal(int currentVoteNumber, List<Proposal> proposals) {
        int voteNumber = currentVoteNumber + 1;
        if (proposals.isEmpty())
            return new Proposal(voteNumber, PROPOSALS[RANDOM.nextInt(PROPOSALS.length)]);
        Collections.sort(proposals);
        Proposal maxVote = proposals.get(proposals.size() - 1);
        long maxVoteNumber = maxVote.voteNumber;
        String content = maxVote.contents;
        if (maxVoteNumber >= currentVoteNumber)
            throw new IllegalStateException("illegal state maxVoteNumber");
        if (content != null)
            return new Proposal(voteNumber, content);
        else return new Proposal(voteNumber, PROPOSALS[RANDOM.nextInt(PROPOSALS.length)]);
    }
    private static class Proposer {



        private static void vote(Proposal proposal, List<Acceptor> acceptors) {
            int quorum = Math.floorDiv(acceptors.size(), 2) + 1;
            int count = 0;
            while (true) {
                printInfo("VOTE_ROUND","START","" + count);
                List<Proposal> proposals = new ArrayList<>();
                for(Acceptor acceptor:acceptors){
                    Promise promise = acceptor.onPrepare(proposal);
                    if(promise != null && promise.ack){
                        proposals.add(promise.proposal);
                    }
                }
                if(proposals.size() < quorum){
                    printInfo("PROPOSER[" + proposal + "]","VOTE","NOT PREPARED" );
                    proposal = nextProposal(proposal.voteNumber, proposals);
                    continue;
                }
                int acceptCount = 0;
                for(Acceptor acceptor:acceptors){
                    if(acceptor.onAccept(proposal)){
                        acceptCount++;
                    }
                }
                if(acceptCount < quorum){
                    printInfo("PROPOSER[" + proposal + "]","VOTE","NOT ACCEPTED" );
                    proposal = nextProposal(proposal.voteNumber, proposals);
                    continue;
                }
                printInfo("PROPOSER[" + proposal + "]","VOTE","SUCCESS" );
                break;
            }
        }
    }

    private static class Promise {

        private final boolean ack;
        private final Proposal proposal;

        public Promise(boolean ack, Proposal proposal) {
            this.ack = ack;
            this.proposal = proposal;
        }

        public boolean isAck() {
            return ack;
        }

        public Proposal getProposal() {
            return proposal;
        }
    }

    private static class Acceptor {
        String name;
        Proposal last  = new Proposal();

        public Acceptor(String name) {
            this.name = name;
        }

        private Promise onPrepare(Proposal proposal){
            if(RANDOM.nextInt() < 0.5){
                printInfo("ACCEPTOR",name,"NO RESPONSE");
                return null;
            }
            if(proposal == null)
                throw new IllegalArgumentException("error state");
            if(proposal.voteNumber <= last.voteNumber){
                printInfo("ACCPETOR",name,"REJECT");
                return new Promise(false, null);
            }else{
                Promise response = new Promise(true, last);
                last = proposal;
                printInfo("ACCPETOR",name,"OK");
                return response;
            }

        }

        private boolean onAccept(Proposal proposal) {
            //假设这个过程有50%的几率失败
            if (Math.random() - 0.5 > 0) {
                printInfo("ACCEPTER_" + name, "ACCEPT", "NO RESPONSE");
                return false;
            }
            printInfo("ACCEPTER_" + name, "ACCEPT", "OK");
            return last.equals(proposal);
        }
    }

    private static class Proposal implements Comparable<Proposal> {
        int voteNumber;
        String contents;

        public Proposal(int voteNumber, String contents) {
            this.voteNumber = voteNumber;
            this.contents = contents;
        }

        public Proposal() {
        }

        @Override
        public String toString() {
            return "Proposal{" +
                    "voteNumber=" + voteNumber +
                    ", contents='" + contents + '\'' +
                    '}';
        }

        @Override
        public int compareTo(Proposal o) {
            return Long.compare(voteNumber, o.voteNumber);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Proposal proposal = (Proposal) o;
            return voteNumber == proposal.voteNumber &&
                    Objects.equals(contents, proposal.contents);
        }

        @Override
        public int hashCode() {
            return Objects.hash(voteNumber, contents);
        }
    }
}
