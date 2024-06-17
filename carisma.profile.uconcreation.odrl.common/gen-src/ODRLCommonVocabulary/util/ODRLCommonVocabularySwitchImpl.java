package ODRLCommonVocabulary.util;

import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

import ODRLCommonVocabulary.Asset;
import ODRLCommonVocabulary.AssetCollection;
import ODRLCommonVocabulary.AssetRelation;
import ODRLCommonVocabulary.ConstrainableElement;
import ODRLCommonVocabulary.Constraint;
import ODRLCommonVocabulary.Duty;
import ODRLCommonVocabulary.LogicalConstraint;
import ODRLCommonVocabulary.ODRLPolicy;
import ODRLCommonVocabulary.Party;
import ODRLCommonVocabulary.PartyCollection;
import ODRLCommonVocabulary.PartyFunction;
import ODRLCommonVocabulary.Permission;
import ODRLCommonVocabulary.Prohibition;
import ODRLCommonVocabulary.RefinableElement;
import ODRLCommonVocabulary.Rule;

public class ODRLCommonVocabularySwitchImpl<T> extends ODRLCommonVocabularySwitch<T> {
	
	
	@Override
	protected T doSwitch(EClass eClass, EObject eObject) {
		System.out.println("In doSwitch(2)");
		System.out.println(eClass.getEPackage());
		if (isSwitchFor(eClass.getEPackage()))
	    {
			System.out.println("is for package");
	      return doSwitch(eClass.getClassifierID(), eObject);
	    }
	    else
	    {
	    	System.out.println("is not for package");
	      List<EClass> eSuperTypes = eClass.getESuperTypes();
	      return eSuperTypes.isEmpty() ? defaultCase(eObject) : doSwitch(eSuperTypes.get(0), eObject);
	    }
	}

	@Override
	public T doSwitch(EObject eObject) {
		System.out.println("In doSwitch(1)");
		return doSwitch(eObject.eClass(), eObject);
	}

	@Override
	public T caseODRLPolicy(ODRLPolicy object) {
		System.out.println("in policy case");
		System.out.println(object.getUid());
		return super.caseODRLPolicy(object);
	}

	@Override
	public T caseRule(Rule object) {
		// TODO Auto-generated method stub
		return super.caseRule(object);
	}

	@Override
	public T caseRefinableElement(RefinableElement object) {
		// TODO Auto-generated method stub
		return super.caseRefinableElement(object);
	}

	@Override
	public T caseLogicalConstraint(LogicalConstraint object) {
		// TODO Auto-generated method stub
		return super.caseLogicalConstraint(object);
	}

	@Override
	public T caseConstraint(Constraint object) {
		// TODO Auto-generated method stub
		return super.caseConstraint(object);
	}

	@Override
	public T caseConstrainableElement(ConstrainableElement object) {
		// TODO Auto-generated method stub
		return super.caseConstrainableElement(object);
	}

	@Override
	public T casePartyFunction(PartyFunction object) {
		// TODO Auto-generated method stub
		return super.casePartyFunction(object);
	}

	@Override
	public T caseParty(Party object) {
		// TODO Auto-generated method stub
		return super.caseParty(object);
	}

	@Override
	public T casePermission(Permission object) {
		// TODO Auto-generated method stub
		return super.casePermission(object);
	}

	@Override
	public T caseDuty(Duty object) {
		// TODO Auto-generated method stub
		return super.caseDuty(object);
	}

	@Override
	public T caseProhibition(Prohibition object) {
		// TODO Auto-generated method stub
		return super.caseProhibition(object);
	}

	@Override
	public T caseAsset(Asset object) {
		// TODO Auto-generated method stub
		return super.caseAsset(object);
	}

	@Override
	public T caseAssetCollection(AssetCollection object) {
		// TODO Auto-generated method stub
		return super.caseAssetCollection(object);
	}

	@Override
	public T casePartyCollection(PartyCollection object) {
		// TODO Auto-generated method stub
		return super.casePartyCollection(object);
	}

	@Override
	public T caseAssetRelation(AssetRelation object) {
		// TODO Auto-generated method stub
		return super.caseAssetRelation(object);
	}

	@Override
	public T defaultCase(EObject object) {
		System.out.println("In default case");
		return super.defaultCase(object);
	}
	
}
