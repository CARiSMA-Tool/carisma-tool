package carisma.check.sequencediagramcrypto.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import carisma.check.sequencediagramcrypto.knowledge.BiFunctionKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.ConcKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.DecKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.EncKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.ExtKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.FunctionKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.InvKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.check.sequencediagramcrypto.knowledge.SignKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.SymdecKnowledge;
import carisma.check.sequencediagramcrypto.knowledge.SymencKnowledge;

public class KnowledgeBase extends HashSet<Knowledge> implements Set<Knowledge> {
	
	private static final long		serialVersionUID	= -9100265947751105327L;
	
	private List<Set<Knowledge>>	equivalenceClasses	= new ArrayList<Set<Knowledge>>();
	
	public boolean knows(Knowledge value) {
		value = value.simplify();
		
		boolean knows = this.contains(value);
		if (knows) {
			return true;
		}
		
		if (value instanceof ConcKnowledge) {
			knows |= this.knowsAxiom238((ConcKnowledge) value);
		}
		else if (value instanceof EncKnowledge) {
			knows |= this.knowsAxiom239((EncKnowledge) value);
		}
		else if (value instanceof SymencKnowledge) {
			knows |= this.knowsAxiom2310((SymencKnowledge) value);
		}
		else if (value instanceof DecKnowledge) {
			knows |= this.knowsAxiom2311((DecKnowledge) value);
		}
		else if (value instanceof SymdecKnowledge) {
			knows |= this.knowsAxiom2312((SymdecKnowledge) value);
		}
		else if (value instanceof ExtKnowledge) {
			knows |= this.knowsAxiom2313((ExtKnowledge) value);
		}
		else if (value instanceof SignKnowledge) {
			knows |= this.knowsAxiom2314((SignKnowledge) value);
		}
		
		if (knows) {
			return true;
		}
		knows |= this.knowsAxiom2315(value);
		if (knows) {
			return true;
		}
		knows |= this.knowsAxiom2316(value);
		if (knows) {
			return true;
		}
		knows |= this.knowsAxiom2317(value);
		if (knows) {
			return true;
		}
		knows |= this.knowsAxiom2318(value);
		if (knows) {
			return true;
		}
		return this.knowsAxiom2319(value);
	}
	
	// Axiom 2.3.8: knows(a) & knows(b) => knows(conc(a,b))
	private boolean knowsAxiom238(ConcKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.9: knows(a) & knows(b) => knows(enc(a,b))
	private boolean knowsAxiom239(EncKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.10: knows(a) & knows(b) => knows(symenc(a,b))
	private boolean knowsAxiom2310(SymencKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.11: knows(a) & knows(b) => knows(dec(a,b))
	private boolean knowsAxiom2311(DecKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.12: knows(a) & knows(b) => knows(syndec(a,b))
	private boolean knowsAxiom2312(SymdecKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.13: knows(a) & knows(b) => knows(ext(a,b))
	private boolean knowsAxiom2313(ExtKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.14: knows(a) & knows(b) => knows(sign(a,b))
	private boolean knowsAxiom2314(SignKnowledge value) {
		boolean knows = this.knows(value.first()) && this.knows(value.second());
		if (knows) {
			this.add(value);
		}
		return knows;
	}
	
	// Axiom 2.3.15: knows(conc(a,b)) => knows(a) & knows(b)
	private boolean knowsAxiom2315(Knowledge aORb) {
		for (Knowledge know : this) {
			if (know instanceof ConcKnowledge) {
				ConcKnowledge conc = (ConcKnowledge) know;
				if (this.isSame(conc.first(), aORb) || this.isSame(conc.second(), aORb)) {
					this.add(aORb);
					return true;
				}
			}
		}
		return false;
	}
	
	// Axiom 2.3.16: knows(symenc(a,k)) & knows(k) => knows(a)
	private boolean knowsAxiom2316(Knowledge value) {
		for (Knowledge know : this) {
			if (know instanceof SymencKnowledge) {
				SymencKnowledge enc = (SymencKnowledge) know;
				if (this.isSame(enc.first(), value)) {
					if (this.knows(enc.second())) {
						this.add(value);
						return true;
					}
				}
			}
		}
		return false;
	}
	
	// Axiom 2.3.17: knows(symdec(a,k)) & knows(k) => knows(a)
	private boolean knowsAxiom2317(Knowledge value) {
		for (Knowledge know : this) {
			if (know instanceof SymdecKnowledge) {
				SymdecKnowledge dec = (SymdecKnowledge) know;
				if (this.knows(dec.second())) {
					if (this.isSame(dec.first(), value)) {
						this.add(value);
						return true;
					}
				}
			}
		}
		return false;
	}
	
	// Axiom 2.3.18: knows(enc(a,k)) & knows(inv(k)) => knows(a)
	private boolean knowsAxiom2318(Knowledge value) {
		for (Knowledge know : this) {
			if (know instanceof EncKnowledge) {
				EncKnowledge enc = (EncKnowledge) know;
				if (enc.second() instanceof InvKnowledge) {
					if (this.knows(((InvKnowledge) enc.second()).first())) {
						if (this.isSame(enc.first(), value)) {
							this.add(value);
							return true;
						}
					}
				}
				else {
					if (this.contains(new InvKnowledge(enc.second()))) {
						if (this.isSame(enc.first(), value)) {
							this.add(value);
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	// Axiom 2.3.19: knows(sign(a,k)) & knows(k) => knows(a)
	private boolean knowsAxiom2319(Knowledge value) {
		for (Knowledge know : this) {
			if (know instanceof SignKnowledge) {
				SignKnowledge sign = (SignKnowledge) know;
				if (this.knows(sign.second())) {
					if (this.isSame(sign.first(), value)) {
						this.add(value);
						return true;
					}
				}
			}
		}
		return false;
	}
	
	public boolean isSame(Knowledge knowledge, Knowledge knowledge2) {
		
		Knowledge knowledgeSimp = knowledge.simplify();
		this.setSame(knowledge, knowledgeSimp);
		Knowledge knowledgeSimp2 = knowledge2.simplify();
		this.setSame(knowledge2, knowledgeSimp2);
		
		//Check whether or not both facts are syntactical equivalent
		if(knowledge.equals(knowledge2) || knowledgeSimp.equals(knowledgeSimp2)){
			this.setSame(knowledge, knowledge2);
			return true;
		}
		
		if(knowledge.getClass().equals(knowledge2.getClass())){
			if (knowledge instanceof BiFunctionKnowledge && knowledge2 instanceof BiFunctionKnowledge){
				BiFunctionKnowledge kBi1 = (BiFunctionKnowledge) knowledge ;
				BiFunctionKnowledge kBi2 = (BiFunctionKnowledge) knowledge2 ;
				if(this.isSame(kBi1.first(), kBi2.first()) && this.isSame(kBi1.second(), kBi2.second())){
					this.setSame(knowledge, knowledge2);
					return true;
				}
			} else if (knowledge instanceof FunctionKnowledge && knowledge2 instanceof FunctionKnowledge){
				FunctionKnowledge kF1 = (FunctionKnowledge) knowledge ;
				FunctionKnowledge kF2 = (FunctionKnowledge) knowledge2 ;
				if(this.isSame(kF1.first(), kF2.first())){
					this.setSame(knowledge, knowledge2);
					return true;
				}
			} 
		}
		
		//Check if a fact is in any eqClass, if so, the other facts also needs to 
		//be in the eqClass, otherwise they cant be equal/the same
		for (Set<Knowledge> eqClass : this.equivalenceClasses) {
			boolean containsKnowledge = eqClass.contains(knowledge);
			boolean containsKnowledge2 = eqClass.contains(knowledge2);
			boolean containsKnowledgeSimp = eqClass.contains(knowledgeSimp);
			boolean containsKnowledgeSimp2 = eqClass.contains(knowledgeSimp2);
			if (containsKnowledge || containsKnowledge2 || containsKnowledgeSimp
					|| containsKnowledgeSimp2) {
				return (containsKnowledge && containsKnowledge2)
						|| (containsKnowledgeSimp && containsKnowledgeSimp2);
			}
		}
		return false;
	}
	
	public void setSame(Knowledge knowledge, Knowledge knowledge2) {
		
		Set<Knowledge> sameSet = new HashSet<Knowledge>();
		{
			sameSet.add(knowledge);
			sameSet.add(knowledge.simplify());
			sameSet.add(knowledge2);
			sameSet.add(knowledge2.simplify());
		}
		
		for (Set<Knowledge> eqClass : this.equivalenceClasses) {
			if(!Collections.disjoint(eqClass, sameSet)){
				//Found eqClass that contains at least one element of sameSet!
				eqClass.addAll(sameSet);
				this.minimizeEquivalenceRelation();
				return;
			}
		}
		
		//At this point not eqClass that contains at lease one element of sameSet was found!
		//Add the sameSet as eqClass
		this.equivalenceClasses.add(sameSet);
		
		if(knowledge.getClass().equals(knowledge2.getClass())){
			if (knowledge instanceof BiFunctionKnowledge && knowledge2 instanceof BiFunctionKnowledge) {
				BiFunctionKnowledge biFun1 = (BiFunctionKnowledge) knowledge;
				BiFunctionKnowledge biFun2 = (BiFunctionKnowledge) knowledge2;
				if (biFun1.getClass().equals(biFun2.getClass())) {
					this.setSame(biFun1.first(), biFun2.first());
					this.setSame(biFun1.second(), biFun2.second());
				}
			}
			if (knowledge instanceof FunctionKnowledge && knowledge2 instanceof FunctionKnowledge) {
				FunctionKnowledge fun1 = (FunctionKnowledge) knowledge;
				FunctionKnowledge fun2 = (FunctionKnowledge) knowledge2;
				if (fun1.getClass().equals(fun2.getClass())) {
					this.setSame(fun1.first(), fun2.first());
				}
			}
		}
		
	}
	
	private void minimizeEquivalenceRelation() {
		
		// Merge equivalenceclasses
		for (Set<Knowledge> eqClass1 : this.equivalenceClasses) {
			for (Set<Knowledge> eqClass2 : this.equivalenceClasses) {
				if(eqClass1 != eqClass2 && !Collections.disjoint(eqClass1, eqClass2)){
					//Both eqClass have at least one element in common, therefore merge them
					eqClass1.addAll(eqClass2);
					eqClass2.clear();
				}
			}
		}
		
		// Remove empty classes
		Iterator<Set<Knowledge>> iterator = this.equivalenceClasses.iterator();
		while (iterator.hasNext()) {
			Set<Knowledge> eqClass = iterator.next();
			if (eqClass.isEmpty()) {
				iterator.remove();
			}
		}
	}
	
	@Override
	public boolean add(Knowledge e) {
		boolean wasAdded = super.add(e);
		if (wasAdded) {
			this.conclusion();
		}
		return wasAdded;
	}
	
	@Override
	public String toString() {
		return "KnowledgeBase [ " + super.toString() + " ]" + "\n" + "EquivalenceClasses ["
				+ equivalenceClasses + "]";
	}
	
	public void conclusion() {
		
		Set<Knowledge> currentKnowledge = new HashSet<Knowledge>(this.size());
		currentKnowledge.addAll(this);
		{
			// Conclusion
			for (Knowledge a : currentKnowledge) {
				
				// Axiom 2.2.2.1: dec(enc(a, k), inv(k)) = a
				if (a instanceof DecKnowledge) {
					DecKnowledge dec = (DecKnowledge) a;
					if (dec.first() instanceof EncKnowledge) {
						EncKnowledge enc = (EncKnowledge) dec.first();
						if (dec.second() instanceof InvKnowledge) {
							InvKnowledge decInv = (InvKnowledge) dec.second();
							if (this.isSame(decInv.first(), enc.second())) {
								this.add(enc.first());
								this.setSame(a, enc.first());
							}
						}
						else if (this.isSame(dec.second(), new InvKnowledge(enc.second()))) {
							this.add(enc.first());
							this.setSame(a, enc.first());
						}
					}
				}
				else
				// Axiom 2.1.2.2: symdec(symenc(a, k), k) = a
				if (a instanceof SymdecKnowledge) {
					SymdecKnowledge symdec = (SymdecKnowledge) a;
					if (symdec.first() instanceof SymencKnowledge) {
						SymencKnowledge symenc = (SymencKnowledge) symdec.first();
						if (this.isSame(symdec.second(), symenc.second())) {
							this.add(symenc.first());
							this.setSame(a, symenc.first());
						}
					}
				}
				else
				// Axiom 2.1.2.3: ext(sign(a,inv(k)),k) = a
				if (a instanceof ExtKnowledge) {
					ExtKnowledge ext = (ExtKnowledge) a;
					if (ext.first() instanceof SignKnowledge) {
						SignKnowledge sign = (SignKnowledge) ext.first();
						if (sign.second() instanceof InvKnowledge) {
							InvKnowledge innerInv = (InvKnowledge) sign.second();
							if (this.isSame(ext.second(), innerInv.first())) {
								this.add(sign.first());
								this.setSame(a, sign.first());
							}
						}
					}
				}
				else
				// Axiom 2.1.2.4: inv(inv(k)) = k
				if (a instanceof InvKnowledge) {
					InvKnowledge inv = (InvKnowledge) a;
					if (inv.first() instanceof InvKnowledge) {
						InvKnowledge innerInv = ((InvKnowledge) inv.first());
						this.add(innerInv.first());
						this.setSame(a, innerInv.first());
					}
				}
				else
				// Axiom 2.1.2.5: conc(a,conc(b,c)) = conc(conc(a,b),c)
				if (a instanceof ConcKnowledge) {
					ConcKnowledge conc = (ConcKnowledge) a;
					if (conc.first() instanceof ConcKnowledge) {
						ConcKnowledge innerConc = (ConcKnowledge) conc.first();
						ConcKnowledge eqConc = new ConcKnowledge(innerConc.first(),
								new ConcKnowledge(innerConc.second(), conc.second()));
						this.add(eqConc);
						this.setSame(a, eqConc);
					}
					else if (conc.second() instanceof ConcKnowledge) {
						ConcKnowledge innerConc = (ConcKnowledge) conc.second();
						ConcKnowledge eqConc = new ConcKnowledge(new ConcKnowledge(conc.first(),
								innerConc.first()), innerConc.second());
						this.add(eqConc);
						this.setSame(a, eqConc);
					}
				}
				
				// Axiom 2.3.15: knows(conc(a,b)) => knows(a) & knows(b)
				if (a instanceof ConcKnowledge) {
					ConcKnowledge conc = (ConcKnowledge) a;
					this.add(conc.first());
					this.add(conc.second());
				}
				else
				// Axiom 2.3.16: knows(symenc(a,k)) & knows(k) => knows(a)
				if (a instanceof SymencKnowledge) {
					SymencKnowledge symenc = (SymencKnowledge) a;
					if (this.knows(symenc.second())) {
						this.add(symenc.first());
					}
				}
				else
				// Axiom 2.3.17: knows(symdec(a,k)) & knows(k) => knows(a)
				if (a instanceof SymdecKnowledge) {
					SymdecKnowledge symdec = (SymdecKnowledge) a;
					if (this.knows(symdec.second())) {
						this.add(symdec.first());
					}
				}
				else
				// Axiom 2.3.18: knows(enc(a,k)) & knows(inv(k)) => knows(a)
				if (a instanceof EncKnowledge) {
					EncKnowledge enc = (EncKnowledge) a;
					if (this.knows(new InvKnowledge(enc.second()))) {
						this.add(enc.first());
					}
				}
				else
				// Axiom 2.3.19: knows(sign(a,k)) & knows(k) => knows(a)
				if (a instanceof SignKnowledge) {
					SignKnowledge sign = (SignKnowledge) a;
					if (this.knows(sign.second())) {
						this.add(sign.first());
					}
				}
				
			}
		}
		
	}
	
	@Deprecated
	public void expansion() {
		
		Set<Knowledge> currentKnowledge = new HashSet<Knowledge>(this.size());
		currentKnowledge.addAll(this);
		{
			// Expansion
			List<Knowledge> knowledgeToAdd = new ArrayList<Knowledge>();
			{
				for (Knowledge a : currentKnowledge) {
					for (Knowledge b : currentKnowledge) {
						// Axiom 2.3.8: knows(a) & knows(b) => knows(conc(a,b))
						knowledgeToAdd.add(new ConcKnowledge(a, b));
						
						// Axiom 2.3.9: knows(a) & knows(b) => knows(enc(a,b))
						knowledgeToAdd.add(new EncKnowledge(a, b));
						
						// Axiom 2.3.10: knows(a) & knows(b) =>
						// knows(symenc(a,b))
						knowledgeToAdd.add(new SymencKnowledge(a, b));
						
						// Axiom 2.3.11: knows(a) & knows(b) => knows(dec(a,b))
						knowledgeToAdd.add(new DecKnowledge(a, b));
						
						// Axiom 2.3.12: knows(a) & knows(b) =>
						// knows(syndec(a,b))
						knowledgeToAdd.add(new SymdecKnowledge(a, b));
						
						// Axiom 2.3.13: knows(a) & knows(b) => knows(ext(a,b))
						knowledgeToAdd.add(new ExtKnowledge(a, b));
						
						// Axiom 2.3.14: knows(a) & knows(b) => knows(sign(a,b))
						knowledgeToAdd.add(new SignKnowledge(a, b));
					}
				}
			}
			this.addAll(knowledgeToAdd);
		}
		
	}
	
}
